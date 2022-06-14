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
data Local = Local 
  { localName :: T.Text
  , localDepth :: Int 
  , localReady :: Bool}
-- as much as i'd love to reuse my env impl, it doesn't lend itself well to compilation
data CompileState = CompileState
  { _locals :: [Local]
  , _scopeDepth :: Int 
  }

makeLenses ''CompileState

compile :: [Stmt] -> Either CompileError B.ByteString
compile = run . evalState (CompileState [] 0) . runError . execBuild . compilePut

compilePut :: CompMembers r => [Stmt] -> Sem r ()
compilePut = 
  foldr ((*>) . compileStmt) (pure ())

compileStmt :: CompMembers r => Stmt -> Sem r ()
compileStmt (Stmt (Eval e) _) = compileExpr e *> build OpPop
compileStmt (Stmt (Print e) _) = compileExpr e *> build OpPrint
compileStmt (Stmt (VarDef name def) pos) = do
  declareVariable pos name
  case def of 
    Nothing -> build OpNil
    Just v -> compileExpr v
  defineVariable name 

compileStmt (Stmt (Block ss) pos) = beginScope *> compilePut ss *> endScope

compileStmt (Stmt (LIf e ifstmt estmt) pos) = do -- todo: broken 
  compileExpr e
  daIfBytes <- execBuild (compileStmt ifstmt)
  let ifByteLen = B.length daIfBytes
  build OpJumpIfFalse
  if ifByteLen > (2 ^ 15) - 5 then
    tooMuchToJump pos
  else do 
    case estmt of 
      Nothing -> do
        buildRaw (int16BE (fromIntegral ifByteLen))
        build OpPop
        buildRaw (lazyByteString daIfBytes)
      Just selse -> do
        buildRaw (int16BE (fromIntegral ifByteLen + 5))
        build OpPop
        daElseBytes <- execBuild (compileStmt selse)
        let elseByteLen = B.length daElseBytes
        if elseByteLen > 2 ^ 15 - 1 then
          tooMuchToJump pos
        else do 
          build OpJump
          buildRaw (int16BE (fromIntegral elseByteLen + 1))
          build OpPop
          buildRaw (lazyByteString daElseBytes)

compileStmt (Stmt (LWhile e loop) pos) = do 
  daLoop <- execBuild (do 
    compileExpr e
    daBody <- execBuild (do 
      build OpPop 
      compileStmt loop) 
    let bodyLen = B.length daBody
    if bodyLen > 2 ^ 15 then
      tooMuchToJump pos
    else do 
      build OpJumpIfFalse
      buildRaw (int16BE (fromIntegral bodyLen + 1))
      build OpPop
      buildRaw (lazyByteString daBody))
  let loopLen = B.length daLoop
  if loopLen > 2 ^ 15 then 
    tooMuchToJump pos 
  else do
    buildRaw (lazyByteString daLoop)
    build OpJump
    buildRaw (int16BE (negate (fromIntegral loopLen + jumpOpSize)))
    build OpPop

compileStmt (Stmt (LFunDecl' name args ss) pos) = do
  -- pass through errors (and state... test this)
  ssbytes <- execBuild (beginScope *> compilePut ss *> endScope)
  build (ObjFunction (length args) (Chunk ssbytes) name)

-- todo: ensure it's actually returnable
compileStmt (Stmt (LReturn e) _) = do
  case e of 
    Nothing -> build OpNil
    Just v  -> compileExpr v

  build OpReturn
  
compileExpr :: CompMembers r => Expr -> Sem r ()
compileExpr (Expr (Literal v) _) = 
  case v of 
    LoxString t -> build OpConstant *> build (ValString t)
    LoxNil      -> build OpNil
    LoxNumber d -> build OpConstant *> build (ValNumber d)
    LoxBool True -> build OpTrue
    LoxBool False -> build OpFalse
    _ -> undefined -- unreachable

compileExpr (Expr (Unary k v) _) =  compileExpr v *> 
  case k of 
    Not -> build OpNot
    Negate -> build OpNegate

compileExpr (Expr (Binop l r And) pos) = do 
  compileExpr l
  daOtherPart <- execBuild (do
                    build OpPop
                    compileExpr r)
  let rexprLen = B.length daOtherPart
  build OpJumpIfFalse
  if rexprLen > 2 ^ 15 then
    tooMuchToJump pos
  else do
    buildRaw (int16BE (fromIntegral rexprLen))
    buildRaw (lazyByteString daOtherPart)
compileExpr (Expr (Binop l r Or) pos) = do 
  compileExpr l
  build OpJumpIfFalse
  buildRaw (word16BE 5) -- length of opcode + Word16
  daOtherPart <- execBuild (do 
                    build OpPop
                    compileExpr r)
  let rexprLen = B.length daOtherPart
  build OpJump
  if rexprLen > 2 ^ 15 then 
    tooMuchToJump pos
  else do 
    buildRaw (int16BE (fromIntegral rexprLen))
    buildRaw (lazyByteString daOtherPart)

    

compileExpr (Expr (Binop l r k) _) = compileExpr l *> compileExpr r *> 
  case k of 
    Equals -> build OpEqual
    Unequal -> build OpEqual *> build OpNot
    Greater -> build OpGreater
    GreaterEq -> build OpLess *> build OpNot
    Less -> build OpLess
    LessEq -> build OpGreater *> build OpNot
    Plus -> build OpAdd
    Minus -> build OpSubtract
    Times -> build OpMultiply
    Divide -> build OpDivide

compileExpr (Expr (Identifier name) pos) = namedVariable pos False name

compileExpr (Expr (Assign name re) pos) = compileExpr re *> namedVariable pos True name

compileExpr (Expr (Grouping e) _) = compileExpr e

compileExpr (Expr (AnonFun{}) pos) = compileError pos "unimplemented"
defineVariable :: CompMembers r => T.Text -> Sem r ()
defineVariable name = do 
  curDepth <- use scopeDepth
  if curDepth == 0 then 
    build OpDefineGlobal *> build name
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
markReady = modifying (locals._head) (\l -> l { localReady = True} )
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
      getOp = if existsLocal then OpGetLocal else OpSetGlobal
  build (if assign then setOp else getOp) *> build name

beginScope :: CompMembers r => Sem r ()
beginScope = scopeDepth += 1

endScope :: CompMembers r => Sem r ()
endScope = do 
  scopeDepth -= 1
  (newLocals, removed) <- popAll <$> use locals <*> use scopeDepth <*> pure 0
  replicateM_  removed (build OpPop)
  assign locals newLocals
  where 
    popAll :: [Local] -> Int -> Int -> ([Local], Int)
    popAll ls@(Local _ d _:xs) depth n =
      if d > depth then 
        popAll xs depth (n + 1) 
      else (ls, n)
    popAll [] _ n = ([], n)
type CompMembers r = Members [State CompileState, Build, Error CompileError] r
data CompileError 
  = JumpSizeError SourcePos 
  | CompError SourcePos T.Text
showCompileError :: CompileError -> String
showCompileError (JumpSizeError pos) = "Error at " ++ sourcePosPretty pos ++ ": Too much code to jump over"
showCompileError (CompError pos txt) = "Error at " ++ sourcePosPretty pos ++ ": " ++ T.unpack txt
jumpOpSize :: (Integral a) => a
jumpOpSize = 5
tooMuchToJump :: CompMembers r => SourcePos -> Sem r a
tooMuchToJump pos = throw (JumpSizeError pos)

compileError :: CompMembers r => SourcePos -> T.Text -> Sem r a
compileError pos name = throw (CompError pos name)
