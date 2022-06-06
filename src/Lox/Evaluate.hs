{-# LANGUAGE RecursiveDo #-}
module Lox.Evaluate (lxRun, lxEvaluate, lxStmts, lxStmt) where 

import Lox.Types
import Lox.Resolver qualified as R
import Data.Text qualified as T
import Polysemy
import Data.Time.Clock.System
import Polysemy.StackState
import Data.HashMap.Lazy qualified as HM
import Polysemy.Fail
import Polysemy.Error
import Polysemy.Fixpoint
import Prelude 
import Data.Bifunctor qualified as BFu
import Data.Functor (($>))
import Control.Monad ((=<<), void, join)
import Control.Monad.Fix (mfix)
import Data.Foldable (traverse_)
import Data.Maybe (maybe, fromMaybe)
import Data.Bool (bool)
import Data.Coerce (coerce)
import Lox.Helpers
import Polysemy.Trace
import Debug.Trace (traceIO)
import Data.List (intersperse)
lxRun :: Members LxMembers r => Either [LxStmt] LxExpr -> Sem r ()
lxRun = \case 
    Left stmts -> lxStmts stmts
    Right expr -> lxEvaluate expr >>= trace . prettyLoxValue
lxStmts :: Members LxMembers r =>  [LxStmt] -> Sem r ()
lxStmts = traverse_ lxStmt
lxStmt :: Members LxMembers r => LxStmt -> Sem r ()
lxStmt = \case
  LxExprStmt e -> lxEvaluate e >> pure ()
  LxPrint e -> lxEvaluate e >>= trace . prettyLoxValue
  LxVar n (Just v) -> lxEvaluate v >>= lxDefine n
  LxVar n Nothing -> lxDefine n LvNil
  LxBlock stmts -> lxEnterBlock *> (lxStmts stmts <* lxLeaveBlock)
  LxIf b i e -> bool (maybe (pure ()) lxStmt e) (lxStmt i) . lxTruthy =<< lxEvaluate b
  LxWhile b d -> lxWhile b d
  LxFunDecl decl@(FunDecl n _) -> do
    rec fun <- LvFun decl <$> (lxDefine n fun *> get)
    -- trace . printEnvKeys =<< get
    pure () 
  LxClassDecl name methods -> 
    lxDefine name (LvClass (LoxClass name methods))    
  LxReturn ret -> do 
    env <- get
    throw @ReturnState =<< (,) env <$> maybe (pure LvNil) lxEvaluate ret
lxWhile :: Members LxMembers r => LxExpr -> LxStmt -> Sem r ()
lxWhile e d = 
    bool (pure()) (lxStmt d *> lxWhile e d) . lxTruthy =<< lxEvaluate e
lxDefine :: Member (StackState LxEnv) r => T.Text -> LoxValue -> Sem r ()
lxDefine k v = do 
  poke (\e@(LxEnv m) -> e {variables = HM.insert k v m })

lxGetGlobal :: Members [Fail, StackState LxEnv] r => T.Text -> Sem r LoxValue
lxGetGlobal k = do
  env <- get
  case getPure env k of 
    Just value -> pure value
    Nothing -> fail . T.unpack $ "Undefined variable " <> k
getPure :: [LxEnv] -> T.Text -> Maybe LoxValue
getPure (e@(LxEnv m):es) k = 
  case HM.lookup k m of 
    Just v -> pure v
    Nothing -> 
      getPure es k
getPure [] _ = Nothing
lxGetAt :: Members [Fail, StackState LxEnv] r => Int -> T.Text -> Sem r LoxValue 
lxGetAt dist k = do 
  (LxEnv m) <- ancestorError dist ("LxAnayl - " ++ T.unpack k)
  case HM.lookup k m of 
    Nothing -> fail "Internal error in lxGetAt: Var not found"
    Just v -> pure v
ancestorFail :: Members [Fail, StackState LxEnv] r => Int -> Sem r LxEnv
ancestorFail dist = ancestorError dist "Getting lexical analyser failed"
ancestorError :: Members [Fail, StackState LxEnv] r => Int -> String -> Sem r LxEnv
ancestorError dist err = do
  s <- peekN dist
  case s of 
    Nothing -> fail err
    Just v  -> pure v
ancestorWith :: Members [Fail, StackState LxEnv] r => Int -> (LxEnv -> LxEnv) -> Sem r ()
ancestorWith dist f = ancestorWithError dist f "Assigning lexical anayliser failed"
ancestorWithError :: Members [Fail, StackState LxEnv] r => Int -> (LxEnv -> LxEnv) -> String -> Sem r ()
ancestorWithError dist f err = do 
  s <- peekN dist
  case s of 
    Nothing -> fail err
    _ -> pokeN dist f
lxAssignGlobal :: Members [Fail, StackState LxEnv] r => T.Text -> LoxValue -> Sem r [LxEnv]
lxAssignGlobal k v = do
  lxAssignGlobal_ k v
  get @LxEnv
lxAssignGlobal_ :: Members [Fail, StackState LxEnv] r => T.Text -> LoxValue -> Sem r ()
lxAssignGlobal_ k v = do 
  env <- get
  put @LxEnv =<< lxAssignFail env k v
lxAssignAt :: Members [Fail, StackState LxEnv] r => Int -> T.Text -> LoxValue -> Sem r () 
lxAssignAt dist k v = ancestorWithError dist (coerce (HM.insert k v)) ("LxAnayl - " ++ T.unpack k)
lxAssignFail :: Member Fail r => [LxEnv] -> T.Text -> LoxValue -> Sem r [LxEnv]
lxAssignFail envs k v =
  let e = unsnoc envs in 
    case e of 
      Nothing -> fail "No scopes"
      Just (xs, LxEnv x) -> pure $ snoc xs (LxEnv (HM.insert k v x))
lxEnterBlock :: Member (StackState LxEnv) r => Sem r () 
lxEnterBlock = push $ LxEnv HM.empty 
lxLeaveBlock :: Members [Fail, StackState LxEnv] r => Sem r ()
lxLeaveBlock = do 
  void pop
    
lxEvaluate :: Members LxMembers r  => LxExpr -> Sem r LoxValue
lxEvaluate (LxLit v) = pure $ parseLitToValue v

lxEvaluate (LxGroup e) = lxEvaluate e
lxEvaluate (LxIdent' k d) = 
  case d of 
    Nothing -> lxGetGlobal k
    Just dist -> lxGetAt dist k
lxEvaluate (LxUnary _ t e) = do
  r <- lxEvaluate e
  case t of 
      LxNegate ->  
        LvNumber . negate <$> lxToNum r
      LxNot    ->
        pure $ LvBool (not (lxTruthy r))
lxEvaluate (LxBinop (LxIdent n) re LxAssign) = fail "unreachable"
lxEvaluate (LxEAssign (IdentInfo name depth) expr) = do 
  e <- lxEvaluate expr
  lxAssign depth name e
  pure e

lxEvaluate (LxBinop le re t) = do 
  l <- lxEvaluate le
  case t of 
    LxAnd  -> 
      if lxTruthy l then 
        lxEvaluate re
      else 
        pure l
    LxOr -> 
      if lxTruthy l then 
        pure l 
      else 
        lxEvaluate re
    _ -> lxEvalEquality l re t
lxEvaluate (LxCall callee args) = uncurry3 lxCall =<< (,,) <$> lxEvaluate callee <*> traverse lxEvaluate args <*> pure (getIdent callee)
-- for things that don't short circut
lxEvalEquality :: Members LxMembers r => LoxValue -> LxExpr -> LxBinopKind -> Sem r LoxValue
lxEvalEquality l re t = do 
  r <- lxEvaluate re 
  case t of 
    LxEquals -> pure $ LvBool $ lxEqual l r
    LxUnequal -> pure $ LvBool $ not (lxEqual l r)
    LxPlus -> 
      case (l, r) of 
        (LvNumber ln, LvNumber rn) -> pure $ LvNumber (ln + rn) 
        (LvString ls, LvString rs) -> pure $ LvString (ls <> rs)
        _                          -> fail "Invalid plus usage"
    _ -> 
      lxEvalNumeric l r t
-- for things that need number
lxEvalNumeric :: Members LxMembers r => LoxValue -> LoxValue -> LxBinopKind -> Sem r LoxValue
lxEvalNumeric l r t = do 
  ln <- lxToNum l
  rn <- lxToNum r
  case t of 
    LxMinus     -> pure $ LvNumber $ ln - rn
    LxDiv       -> pure $ LvNumber $ ln / rn
    LxTimes     -> pure $ LvNumber $ ln * rn
    LxGreater   -> pure $ LvBool   $ ln > rn
    LxGreaterEq -> pure $ LvBool   $ ln >= rn
    LxLess      -> pure $ LvBool   $ ln < rn
    LxLessEq    -> pure $ LvBool   $ ln <= rn
    _           -> fail "bad eval"
lxAssign :: Members [Fail, StackState LxEnv] r => Maybe Int -> T.Text -> LoxValue -> Sem r () 
lxAssign depth k v = 
  case depth of 
    Nothing -> lxAssignGlobal_ k v
    Just d  -> lxAssignAt d k v
lxCall :: Members LxMembers r => LoxValue -> [LoxValue] -> Maybe IdentInfo -> Sem r LoxValue
lxCall daValue@(LvFun f@(FunDecl' _ ps ss ) closure) args infos = 
  if compareLengthLs args ps == EQ then do
    oldEnv <- get @LxEnv
    catch @ReturnState (put closure *> runFunDecl args ps ss $> LvNil) (\(newClosure, ret) -> do 
      lxLeaveBlock
      put oldEnv
      case infos of 
        Just (IdentInfo name depth) -> lxAssign depth name (LvFun (FunDecl' name ps ss) newClosure)
        _ -> pure ()
      pure ret)  
  else 
    -- showing length kinda defeats the point of the compareLength
    fail $ "Expected " <> show (length ps) <> " args but got " <> show (length args)
lxCall (LvNativeFun (LoxNativeFun name fun arity)) args _ = 
  if compareLength args arity == EQ then do 
    fun args
  else 
    fail $ "Expected " <> show arity <> " args but got " <> show (length args)
lxCall _ _ _ = fail "Only functions and classes can be called"
runFunDecl :: Members LxMembers r => [LoxValue] -> [T.Text] -> [LxStmt] -> Sem r ()
runFunDecl args ps ss = do
  lxEnterBlock
  traverse_ (uncurry lxDefine) (zip ps args)
  lxStmts ss
  pure ()
   
lxToNum :: Member Fail r => LoxValue -> Sem r Float 
lxToNum (LvNumber n) = pure n
lxToNum (LvString s) = pure . read $ T.unpack s
lxToNum (LvBool True) = pure 1
lxToNum (LvBool False) = pure 0
lxToNum LvNil = fail "Null reference exception"
lxToNum (LvFun{}) = fail "Can't convert function to number"
lxToNum (LvNativeFun{})  = fail "Can't convert function to number"
lxToNum (LvClass{}) = fail "Can't convert class to number"
lxToNum (LvInstance{}) = fail "Can't convert class instance to number"
lxTruthy :: LoxValue -> Bool
lxTruthy LvNil = False
lxTruthy (LvBool b) = b
lxTruthy _ = True
compareLengthLs :: [a] -> [b] -> Ordering
compareLengthLs xs ys = compare (void xs) (void ys)
compareLength :: (Ord b, Num b, Foldable f) => f a -> b -> Ordering
compareLength = foldr (\_ acc n -> if n > 0 then acc (n - 1) else GT) (compare 0)
-- unlike stinky java haskell is smart
lxEqual :: LoxValue -> LoxValue -> Bool 
lxEqual = (==)

uncurry3 f (a, b, c) = f a b c

getIdent :: LxExpr -> Maybe IdentInfo
getIdent (LxIdent infos) = Just infos
getIdent (LxCall callee _) = getIdent callee
getIdent _ = Nothing
