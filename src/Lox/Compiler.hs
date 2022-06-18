{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}
module Lox.Compiler where 

import Lox.Compiler.Chunk
import Lox.Types
import Lox.Helpers
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Builder
import Data.Text qualified as T
import Polysemy.Error
import Polysemy.State hiding (Get)
import Polysemy
import Polysemy.Build
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Control.Lens.TH
import Control.Lens.Setter (set, over)
import Control.Lens.Getter ((^.), view)
import Control.Lens.Cons (_head)
import Control.Lens.Fold ((^?))
import Control.Monad (replicateM_)
import Data.Bifunctor qualified as BFu
import Polysemy.Counter
import Polysemy.Tagged
import Data.Binary (encode, decode)
import Polysemy.ListBuild
data Local = Local 
  { localName :: T.Text
  , localDepth :: Int 
  , localReady :: Bool}
-- as much as i'd love to reuse my env impl, it doesn't lend itself well to compilation
data CompileState = CompileState
  { _locals :: [Local]
  , _scopeDepth :: Int
  , _functionType :: FunctionType
  }

makeLenses ''CompileState

data CounterKind 
  = CountConstants
  | CountChunks
defaultState :: CompileState
defaultState = CompileState [] 0 TypeScript
compileBytes :: [Stmt] -> Either CompileError B.ByteString
compileBytes = BFu.second encode . compile
compile :: [Stmt] -> Either CompileError Chunk
compile stmts = 
  let daRes = run . evalState defaultState . runError . runCounter . untag @CountChunks. runCounter .untag @CountConstants . runListBuild @Obj . execListBuild @OpCode . compileChunk $ stmts in 
    BFu.second (uncurry (flip Chunk)) daRes

compileChunk :: CompMembers r => [Stmt] -> Sem r ()
compileChunk = 
  foldr ((*>) . compileStmt) (pure ())

compileStmt :: CompMembers r => Stmt -> Sem r ()
compileStmt (Stmt (Eval e) _) = compileExpr e *> lsBuild OpPop
compileStmt (Stmt (Print e) _) = compileExpr e *> lsBuild OpPrint
compileStmt (Stmt (VarDef name def) pos) = do
  declareVariable pos name
  case def of 
    Nothing -> lsBuild OpNil
    Just v -> compileExpr v
  defineVariable name 

compileStmt (Stmt (Block ss) pos) = beginScope *> compileChunk ss *> endScope

compileStmt (Stmt (LIf e ifstmt estmt) pos) = do -- todo: broken 
  compileExpr e
  daIfBytes <- execListBuild @OpCode (compileStmt ifstmt)
  let ifByteLen = length daIfBytes
  if ifByteLen > (2 ^ 15) - 1 then
    tooMuchToJump pos
  else do 
    case estmt of 
      Nothing -> do
        lsBuild $ OpJumpIfFalse (fromIntegral ifByteLen)
        lsBuild OpPop
        lsBuildMore daIfBytes
      Just selse -> do
        lsBuild (OpJumpIfFalse (fromIntegral ifByteLen + 1))
        lsBuild OpPop
        lsBuildMore daIfBytes
        daElseBytes <- execListBuild @OpCode (compileStmt selse)
        let elseByteLen = length daElseBytes
        if elseByteLen > 2 ^ 15 - 1 then
          tooMuchToJump pos
        else do 
          lsBuild (OpJump (fromIntegral elseByteLen + 1))
          lsBuild OpPop
          lsBuildMore daElseBytes

compileStmt (Stmt (LWhile e loop) pos) = do 
  daLoop <- execListBuild @OpCode (do 
    compileExpr e
    daBody <- execListBuild @OpCode (do 
      lsBuild OpPop 
      compileStmt loop) 
    let bodyLen = length daBody
    if bodyLen > 2 ^ 15 then
      tooMuchToJump pos
    else do 
      lsBuild $ OpJumpIfFalse (fromIntegral bodyLen + 1)
      lsBuild OpPop
      lsBuildMore daBody)
  let loopLen = length daLoop
  if loopLen > 2 ^ 15 then 
    tooMuchToJump pos 
  else do
    lsBuildMore daLoop
    lsBuild $ OpJump (negate (fromIntegral loopLen + 1))
    lsBuild OpPop

compileStmt (Stmt (LFunDecl' name args ss) pos) = do
  declareVariable pos name
  markReady
  -- pass through errors only
  -- this has the same effect as making a new compiler and running it in c
  -- also allow function counting to pass through LOL
  (consts, opcodes) <- runCounter . untag @CountConstants . evalState (set functionType TypeFunction defaultState)  . runListBuild @Obj $ execListBuild @OpCode (beginScope *> compileChunk ss *> endScope)
  idN <- tag @CountConstants makeCount
  let objFun = ObjFunction (length args) (Chunk opcodes consts) name
  lsBuild (OpConstant (ValObj idN))
  lsBuild @Obj objFun
  defineVariable name
  

-- todo: ensure it's actually returnable
compileStmt (Stmt (LReturn e) _) = do
  case e of 
    Nothing -> lsBuild OpNil
    Just v  -> compileExpr v

  lsBuild OpReturn
  
compileExpr :: CompMembers r => Expr -> Sem r ()
compileExpr (Expr (Literal v) _) = 
  case v of 
    LoxString t -> lsBuild (OpConstant (ValString t))
    LoxNil      -> lsBuild OpNil
    LoxNumber d -> lsBuild (OpConstant (ValNumber d))
    LoxBool True -> lsBuild OpTrue
    LoxBool False -> lsBuild OpFalse
    _ -> undefined -- unreachable

compileExpr (Expr (Unary k v) _) =  compileExpr v *> 
  case k of 
    Not -> lsBuild OpNot
    Negate -> lsBuild OpNegate

compileExpr (Expr (Binop l r And) pos) = do 
  compileExpr l
  daOtherPart <- execListBuild @OpCode (do
                    lsBuild OpPop
                    compileExpr r)
  let rexprLen = length daOtherPart
  if rexprLen > 2 ^ 15 then
    tooMuchToJump pos
  else do
    lsBuild (OpJumpIfFalse (fromIntegral rexprLen))
    lsBuildMore daOtherPart
compileExpr (Expr (Binop l r Or) pos) = do 
  compileExpr l
  lsBuild (OpJumpIfFalse 5) -- length of opcode + Word16
  daOtherPart <- execListBuild @OpCode (do 
                    lsBuild @OpCode OpPop
                    compileExpr r)
  let rexprLen = length daOtherPart
  if rexprLen > 2 ^ 15 then 
    tooMuchToJump pos
  else do
    
    lsBuild (OpJump (fromIntegral rexprLen))
    lsBuildMore daOtherPart

    

compileExpr (Expr (Binop l r k) _) = compileExpr l *> compileExpr r *> 
  case k of 
    Equals -> lsBuild OpEqual
    Unequal -> lsBuild OpEqual *> lsBuild OpNot
    Greater -> lsBuild OpGreater
    GreaterEq -> lsBuild OpLess *> lsBuild OpNot
    Less -> lsBuild OpLess
    LessEq -> lsBuild OpGreater *> lsBuild OpNot
    Plus -> lsBuild OpAdd
    Minus -> lsBuild OpSubtract
    Times -> lsBuild OpMultiply
    Divide -> lsBuild OpDivide

compileExpr (Expr (Identifier name) pos) = namedVariable pos False name

compileExpr (Expr (Assign name re) pos) = compileExpr re *> namedVariable pos True name

compileExpr (Expr (Grouping e) _) = compileExpr e

compileExpr (Expr (AnonFun{}) pos) = compileError pos "unimplemented"
defineVariable :: CompMembers r => T.Text -> Sem r ()
defineVariable name = do 
  curDepth <- use scopeDepth
  if curDepth == 0 then 
    lsBuild $ OpDefineGlobal name
  else do
    markReady
    pure ()
declareVariable :: CompMembers r => SourcePos -> T.Text -> Sem r ()
declareVariable pos name = do 
  curDepth <- use scopeDepth 
  if curDepth == 0 then 
    pure ()
  else do 
    inScope <- localInScope name
    if inScope then 
      throw (CompError pos (name <> " is already in scope"))
    else 
      addLocal name

addLocal :: CompMembers r => T.Text -> Sem r () 
addLocal name = do 
  curDepth <- use scopeDepth
  prepending locals (Local name curDepth False)

markReady :: CompMembers r => Sem r () 
markReady = do
  depth <- use scopeDepth
  if depth == 0 then 
    pure ()
  else
    modifying (locals._head) (\l -> l { localReady = True} )
localInScope :: CompMembers r => T.Text -> Sem r Bool
localInScope name = searchInScope <$> use locals <*> use scopeDepth
  where
    searchInScope :: [Local] -> Int -> Bool
    searchInScope (Local n d _:xs) depth 
      | d < depth = False
      | n == name = True
      | otherwise = searchInScope xs depth
    searchInScope [] _ = False
resolveLocal :: CompMembers r => SourcePos -> T.Text -> Sem r Bool
resolveLocal pos name = do 
  curLocals <- use locals
  searchInScope curLocals
  where 
    searchInScope :: CompMembers r => [Local] -> Sem r Bool
    searchInScope [] = pure False
    searchInScope (Local n d r:xs) 
      | n == name = if r then pure True else compileError pos "Can't read local variable in its own initializer"
      | otherwise = searchInScope xs
namedVariable :: CompMembers r => SourcePos -> Bool -> T.Text -> Sem r () 
namedVariable pos assign name = do 
  existsLocal <- resolveLocal pos name
  let setOp = if existsLocal then OpSetLocal else OpSetGlobal
      getOp = if existsLocal then OpGetLocal else OpGetGlobal
  lsBuild $ (if assign then setOp else getOp) name

beginScope :: CompMembers r => Sem r ()
beginScope = scopeDepth += 1

endScope :: CompMembers r => Sem r ()
endScope = do 
  scopeDepth -= 1
  (newLocals, removed) <- popAll <$> use locals <*> use scopeDepth <*> pure 0
  replicateM_  removed (lsBuild OpPop)
  assign locals newLocals
  where 
    popAll :: [Local] -> Int -> Int -> ([Local], Int)
    popAll ls@(Local _ d _:xs) depth n =
      if d > depth then 
        popAll xs depth (n + 1) 
      else (ls, n)
    popAll [] _ n = ([], n)
type CompMembers r = Members [ListBuild OpCode, ListBuild Obj, State CompileState, Error CompileError, Tagged 'CountConstants Counter, Tagged 'CountChunks Counter] r
data CompileError 
  = JumpSizeError SourcePos 
  | CompError SourcePos T.Text
showCompileError :: CompileError -> String
showCompileError (JumpSizeError pos) = "Error at " ++ sourcePosPretty pos ++ ": Too much code to jump over"
showCompileError (CompError pos txt) = "Error at " ++ sourcePosPretty pos ++ ": " ++ T.unpack txt
tooMuchToJump :: CompMembers r => SourcePos -> Sem r a
tooMuchToJump pos = throw (JumpSizeError pos)

compileError :: CompMembers r => SourcePos -> T.Text -> Sem r a
compileError pos name = throw (CompError pos name)
