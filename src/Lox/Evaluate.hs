module Lox.Evaluate (lxRun, lxEvaluate) where 

import Lox.Types 
import Data.Text qualified as T
import Polysemy
import Data.Time.Clock.System
import Polysemy.State
import Data.HashMap.Strict qualified as HM
import Polysemy.Fail
import Polysemy.Haskeline
import Polysemy.Error
import Prelude 
import Data.Bifunctor qualified as BFu
import Data.Functor (($>))
import Control.Monad ((=<<))
import Data.Foldable (traverse_)
import Data.Maybe (maybe, fromMaybe)
import Data.Bool (bool)
lxRun :: Members [State LxEnv, Fail, Error ReturnState, Haskeline, Embed IO] r => Either [LxStmt] LxExpr -> Sem r ()
lxRun = \case 
    Left stmts -> lxStmts stmts
    Right expr -> lxEvaluate expr >>= outputStrLn . prettyLoxValue
lxStmts :: Members [State LxEnv, Fail, Error ReturnState, Haskeline, Embed IO] r =>  [LxStmt] -> Sem r ()
lxStmts = traverse_ lxStmt
lxStmt :: Members [State LxEnv, Fail, Error ReturnState, Haskeline, Embed IO] r => LxStmt -> Sem r ()
lxStmt = \case
  LxExprStmt e -> lxEvaluate e >> pure ()
  LxPrint e -> lxEvaluate e >>= outputStrLn . prettyLoxValue
  LxVar n (Just v) -> lxEvaluate v >>= lxDefine n
  LxVar n Nothing -> lxDefine n LvNil
  LxBlock stmts -> lxEnterBlock *> (lxStmts stmts <* lxLeaveBlock)
  LxIf b i e -> bool (maybe (pure ()) lxStmt e) (lxStmt i) . lxTruthy =<< lxEvaluate b
  LxWhile b d -> lxWhile b d
  LxFunDecl (FunDecl n (FunInfo ps ss)) -> do
    lxDefine n LvNil
    lxAssign n <$> LvFun . LoxFun n (LoxFunction $ \args -> do 
      traverse_ (uncurry lxDefine) (zip ps args) 
      lxStmts ss) (length ps) =<< get @LxEnv
  LxClassDecl name methods -> 
    lxDefine name (LvClass (LoxClass name methods))    
  LxReturn ret -> throw @ReturnState =<< (,) <$> get @LxEnv <*> lxEvaluate ret
lxWhile :: Members [State LxEnv, Fail, Error ReturnState, Haskeline, Embed IO] r => LxExpr -> LxStmt -> Sem r ()
lxWhile e d = 
    bool (pure()) (lxStmt d *> lxWhile e d) . lxTruthy =<< lxEvaluate e
lxDefine :: Member (State LxEnv) r => T.Text -> LoxValue -> Sem r ()
lxDefine k v = do 
  e@(LxEnv m _ _) <- get
  put @LxEnv $ e { variables = HM.insert k v m } 

lxGet    :: Members [Fail, State LxEnv] r => T.Text -> Sem r LoxValue
lxGet k = do
  env <- get
  case getPure env k of 
    Just (newEnv, value) -> put newEnv $> value
    Nothing -> fail . T.unpack $ "Undefined variable " <> k
getPure :: LxEnv -> T.Text -> Maybe (LxEnv, LoxValue)
getPure s@(LxEnv m enclosing _) k = 
  case HM.lookup k m of 
    Just v -> Just (s, v)
    Nothing -> 
      case enclosing of 
        Nothing -> Nothing
        Just e ->  (,) s . snd <$> getPure e k
  
lxAssign :: Members [Fail, State LxEnv] r => T.Text -> LoxValue -> Sem r ()
lxAssign k v = do 
  env <- get 
  case assignPure env k v of 
    Just newEnv -> put newEnv
    Nothing -> fail . T.unpack $ "Undefined variable " <> k
assignPure :: LxEnv -> T.Text -> LoxValue -> Maybe LxEnv 
assignPure env@(LxEnv m enc _) k v = 
  case HM.lookup k m of 
    Nothing -> 
      case enc of 
        Nothing -> Nothing
        Just e -> case assignPure e k v of 
          Just newEnv -> Just $ env { enclosing = Just newEnv} 
          _ -> Nothing
    Just _ -> Just env { variables = HM.insert k v m }
lxEnterBlock :: Member (State LxEnv) r => Sem r () 
lxEnterBlock = modify $ LxEnv HM.empty <$> Just <*> inFunction 
lxLeaveBlock :: Members [Fail, State LxEnv] r => Sem r ()
lxLeaveBlock = do 
  env <- get 
  case env of 
    LxEnv _ (Just e) _ -> put e
    _ -> fail "Can't leave top scope"
lxEvaluate :: Members [State LxEnv, Fail, Error ReturnState, Haskeline, Embed IO] r  => LxExpr -> Sem r LoxValue
lxEvaluate (LxLit v) = pure $ parseLitToValue v

lxEvaluate (LxGroup e) = lxEvaluate e
lxEvaluate (LxIdent k) = lxGet k
lxEvaluate (LxUnary _ t e) = do
  r <- lxEvaluate e
  case t of 
      LxNegate ->  
        LvNumber . negate <$> lxToNum r
      LxNot    ->
        pure $ LvBool (not (lxTruthy r))
lxEvaluate (LxBinop (LxIdent n) re LxAssign) = do
  env <- get @LxEnv
  r <- lxEvaluate re
  lxAssign n r
  pure r

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
lxEvaluate (LxCall callee args) = uncurry lxCall =<< (,) <$> lxEvaluate callee <*> traverse lxEvaluate args
-- for things that don't short circut
lxEvalEquality :: Members [Fail, State LxEnv, Error ReturnState, Haskeline, Embed IO] r => LoxValue -> LxExpr -> LxBinopKind -> Sem r LoxValue
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
lxEvalNumeric :: Members [Fail, State LxEnv, Haskeline, Embed IO] r => LoxValue -> LoxValue -> LxBinopKind -> Sem r LoxValue
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
     
lxCall :: Members [Fail,State LxEnv, Error ReturnState, Haskeline, Embed IO] r => LoxValue -> [LoxValue] -> Sem r LoxValue
lxCall (LvFun (LoxFun name (LoxFunction fun) n closure)) args = 
  if compareLength args n == EQ then do
    oldEnv <- get @LxEnv
    catch @ReturnState (put closure *> fun args $> LvNil) (\(_, ret) -> put oldEnv $> ret)  
  else 
    -- showing length kinda defeats the point of the compareLength
    fail $ "Expected " <> show n <> " args but got " <> show (length args) 
lxCall _ _ = fail "Only functions and classes can be called"
lxToNum :: Member Fail r => LoxValue -> Sem r Float 
lxToNum (LvNumber n) = pure n
lxToNum (LvString s) = pure . read $ T.unpack s
lxToNum (LvBool True) = pure 1
lxToNum (LvBool False) = pure 0
lxToNum LvNil = fail "Null reference exception"
lxToNum (LvFun{}) = fail "Can't convert function to number"
lxToNum (LvClass{}) = fail "Can't convert class to number"
lxToNum (LvInstance{}) = fail "Can't convert class instance to number"
lxTruthy :: LoxValue -> Bool
lxTruthy LvNil = False
lxTruthy (LvBool b) = b
lxTruthy _ = True

compareLength :: (Ord b, Num b, Foldable f) => f a -> b -> Ordering
compareLength = foldr (\_ acc n -> if n > 0 then acc (n - 1) else GT) (compare 0)
-- unlike stinky java haskell is smart
lxEqual :: LoxValue -> LoxValue -> Bool 
lxEqual = (==)
