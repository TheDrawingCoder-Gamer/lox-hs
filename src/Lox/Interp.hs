module Lox.Interp (interpret, execute, evaluate) where 

import Lox.Types
import Lox.Helpers
import Polysemy hiding (interpret)
import Polysemy.Error 
import Polysemy.Trace
import Polysemy.Reader
import Polysemy.State
import Polysemy.Counter
import Data.Functor ((<&>), ($>))
import Text.Megaparsec (SourcePos)
import Text.Read (readMaybe)
import Data.Maybe (maybe)
import Data.Text qualified as T
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Control.Monad (void, foldM, filterM)
-- rewrite. I am in great pain


interpret :: Members LxMembers r => Either [Stmt] Expr -> Sem r Stack
interpret (Left stmts) = execute stmts
interpret (Right expr) = (trace . prettyLoxValue =<< evaluate expr) *> ask

execute :: Members LxMembers r => [Stmt] -> Sem r Stack
execute [] = ask
execute (Eval expr:rs) = void (evaluate expr) *> execute rs
execute (Print expr:rs) = (trace . prettyLoxValue =<< evaluate expr) *> execute rs
execute (VarDef name init:rs) = do 
  case init of 
    Just i -> do
      i' <- evaluate i
      defineVar name i' (execute rs)
    Nothing -> defineVar name LoxNil (execute rs)
execute (Block ss:rs) = execute ss *> filterInUse *> execute rs
execute (LIf ee sif selse:rs) = do
  e <- evaluate ee
  if valTruthy e then 
    execute [sif]
  else 
    maybe ask (execute . pure) selse
  execute rs
execute (LWhile cond body:rs) = do 
  c <- evaluate cond
  if valTruthy c then 
    execute [body] *> execute [LWhile cond body]
  else 
    execute rs
execute (LFunDecl (FunDecl' name args stmts):rs) = 
  let fun = LoxFun name (length args) 0 args stmts in
    defineFunction name fun (execute rs) 
execute (ClassDecl{}:_) = unexpected
execute (LReturn value:_) = throw . ReturnError =<< maybe (pure LoxNil) evaluate value
execute (StackDump:rs) = (trace . show =<< ask) *> execute rs
execute (HeapDump:rs)  = (trace . show . M.map prettyLoxValue =<< get @Heap) *> execute rs
execute (ClosuresDump:rs) = (trace . show =<< get @FunClosures) *> execute rs
evaluate :: Members LxMembers r => Expr -> Sem r LoxValue
evaluate (Expr (Binop le re And) _ _) = evalBinopLazy le re $ \l re ->
  if valTruthy l then 
    evaluate re
  else 
    pure l
evaluate (Expr (Binop le re Or) _ _) = evalBinopLazy le re $ \l re -> 
  if valTruthy l then
    pure l 
  else
    evaluate re
evaluate (Expr (Binop le re Equals) _ _) = evalBinopStrict le re (\l r -> Right $ LoxBool (l == r)) 
evaluate (Expr (Binop le re Unequal) _ _) = evalBinopStrict le re (\l r -> Right $ LoxBool (l /= r))
evaluate (Expr (Binop le re Plus) pos _) = evalBinopStrict le re (\l r -> 
  case (l, r) of 
    (LoxNumber ln, LoxNumber rn) -> Right $ LoxNumber (ln + rn)
    (LoxString ls, LoxString rs) -> Right $ LoxString (ls <> rs)
    _ -> Left $ InterpreterError pos "Mismatched types on plus operator")
evaluate (Expr (Binop le re Minus) _ _) = evalBinopNumOnly le re (-)
evaluate (Expr (Binop le re Times) _ _) = evalBinopNumOnly le re (*)
evaluate (Expr (Binop le re Divide) _ _) = evalBinopNumOnly le re (/)
evaluate (Expr (Binop le re Greater) _ _) = evalBinopNumGeneric le re (>) LoxBool
evaluate (Expr (Binop le re GreaterEq) _ _) = evalBinopNumGeneric le re (>=) LoxBool
evaluate (Expr (Binop le re Less) _ _) = evalBinopNumGeneric le re (<) LoxBool
evaluate (Expr (Binop le re LessEq) _ _) = evalBinopNumGeneric le re (<=) LoxBool
evaluate (Expr (Unary Not le) _ _) =  evaluate le <&> LoxBool . not . valTruthy
evaluate (Expr (Unary Negate le@(Expr _ pos _)) _ _) =  evaluate le >>= fromEither @InterpError . valToNum pos <&> LoxNumber . negate
evaluate (Expr (Literal l) _ _) = pure l
evaluate (Expr (Grouping e) _ _) = evaluate e
evaluate (Expr (Call callee args) pos _) = do 
  c <- evaluate callee
  as <- traverse evaluate args
  evalCall c as pos
evaluate (Expr (Identifier name) pos _) =  getVar name pos
evaluate (Expr (Assign name expr) pos _) = do 
  val <- evaluate expr
  assignVar pos name val
  pure val
evaluate (Expr (LxParseFail _) _ _) = unexpected
evalBinopLazy   :: Members LxMembers r => Expr -> Expr -> (LoxValue -> Expr -> Sem r LoxValue) -> Sem r LoxValue
evalBinopLazy  le re f = do
  l <- evaluate le
  f l re

evalBinopStrict :: Members LxMembers r => Expr -> Expr -> (LoxValue -> LoxValue -> Either InterpError LoxValue) -> Sem r LoxValue
evalBinopStrict le re f = do 
  l <- evaluate le
  r <- evaluate re
  fromEither $ f l r

evalBinopNum :: Members LxMembers r => Expr -> Expr -> (Double -> Double -> LoxValue) -> Sem r LoxValue
evalBinopNum le@(Expr _ lpos _) re@(Expr _ rpos _) f = do 
  l <- evaluate le
  r <- evaluate re
  ln <- fromEither $ valToNum lpos l
  rn <- fromEither $ valToNum rpos r
  pure $ f ln rn
evalBinopNumGeneric :: Members LxMembers r => Expr -> Expr -> (Double -> Double -> a) -> (a -> LoxValue) -> Sem r LoxValue
evalBinopNumGeneric le re f c = evalBinopNum le re (\l r -> c $ f l r)

evalBinopNumOnly :: Members LxMembers r => Expr -> Expr -> (Double -> Double -> Double) -> Sem r LoxValue
evalBinopNumOnly l r f = evalBinopNumGeneric l r f LoxNumber

evalCall :: Members LxMembers r => LoxValue -> [LoxValue] -> SourcePos -> Sem r LoxValue
evalCall (LoxFun name arity idN params stmts) args pos = do 
  closure <- gets @FunClosures (M.lookup idN)
  case closure of 
    Nothing -> runtimeError pos "Couldn't find closure for function"
    Just cls -> (do
      newStack <- ask >>= defineArgs params args
      runReader cls (local @Stack (M.union newStack) (execute stmts)) $> LoxNil) 
      `catch` 
      \case 
        ReturnError value -> filterInUse $> value 
        e -> throw e
  where 
    defineArgs :: Members LxMembers r => [T.Text] -> [LoxValue] -> Stack -> Sem r Stack
    defineArgs params args env = do 
      (newS, newH) <- foldM (\(s, h) (p, a) -> do
          idN <- makeCount
          pure (M.insert p idN s, M.insert idN a h)
        ) (M.empty, M.empty) (zip params args)  
      modify @Heap (M.union newH)
      pure (M.union newS env)
evalCall _ _ pos = runtimeError pos "Can only call functions"
runtimeError :: Member (Error InterpError) r => SourcePos -> String -> Sem r a
runtimeError p s = throw @InterpError $ InterpreterError p s

unexpected :: Member (Error InterpError) r => Sem r a
unexpected = throw Unexpected

conversionError :: Member (Error InterpError) r => SourcePos -> String -> String -> Sem r a
conversionError pos from = throw . conversionErrorValue pos from

undefinedVarError :: Member (Error InterpError) r => SourcePos -> String -> Sem r a
undefinedVarError pos = throw . InterpreterError pos . ("Undefined variable " ++)
conversionErrorValue :: SourcePos -> String -> String -> InterpError
conversionErrorValue pos from to = InterpreterError pos ("Can't convert " ++ from ++ " to " ++ to)

valTruthy :: LoxValue -> Bool
valTruthy LoxNil = False
valTruthy (LoxBool b) = b
valTruthy _ = True

valToNum :: SourcePos -> LoxValue -> Either InterpError Double
valToNum _ (LoxNumber n) = Right n
valToNum pos (LoxString s) = maybe (Left $ conversionErrorValue pos ("'" ++ T.unpack s ++ "'") "number") Right (readMaybe (T.unpack s))
valToNum _ (LoxBool True) = Right 1
valToNum _ (LoxBool False) = Right 0
valToNum pos LoxNil = Left (conversionErrorValue pos "nil" "number")
valToNum pos LoxFun{} = Left (conversionErrorValue pos "function" "number")
valToNum pos LoxClass{} = Left (conversionErrorValue pos "class" "number")
valToNum pos LoxInstance{} = Left (conversionErrorValue pos "class instance" "number")

getVar :: Members [Reader Stack, State Heap, Error InterpError] r => T.Text -> SourcePos -> Sem r LoxValue
getVar name pos = do 
  stack <- ask
  heap <- get
  case M.lookup name stack of
    Just i -> 
      maybe unexpected pure (M.lookup i heap)
    Nothing ->
      undefinedVarError pos (T.unpack name)

defineVar :: Members [Reader Stack, State Heap, Counter] r => T.Text -> LoxValue -> Sem r a -> Sem r a 
defineVar k v sem =  do 
  idN <- makeCount
  modify (M.insert idN v)
  local (M.insert k idN) sem

defineFunction :: Members [Reader Stack, State Heap, State FunClosures, Counter, Error InterpError] r => T.Text -> LoxValue -> Sem r a -> Sem r a
defineFunction k fn@(LoxFun{}) sem = do 
  idN <- makeCount
  modify @Heap (M.insert idN (fn {funId = idN}) )
  heap <- get @Heap
  stack <- asks (M.insert k idN)
  modify @FunClosures (M.insert idN stack)
  local @Stack (M.insert k idN) sem
defineFunction _ _ _= unexpected
assignVar :: Members [Reader Stack, State Heap, Error InterpError] r => SourcePos -> T.Text -> LoxValue -> Sem r () 
assignVar pos k v = do 
  idN <- asks (M.lookup k)
  case idN of 
    Just idN' -> 
      modify (M.insert idN' v) 
    Nothing -> 
      undefinedVarError pos (T.unpack k)

varInUse :: Members [Reader Stack, State Heap, State FunClosures] r => T.Text -> Sem r Bool
varInUse name = do 
  stack <- ask @Stack 
  if M.member name stack then 
    pure True 
  else do 
    let stackValues = S.fromList (M.elems stack)
    closures <- gets @FunClosures (M.elems . flip M.restrictKeys stackValues)
    pure $ any (M.member name) closures 

idInUse :: Members [Reader Stack, State Heap, State FunClosures] r => Int -> Sem r Bool
idInUse idN = do 
  stackIds <- asks @Stack M.elems
  pure (idN `elem` stackIds) <||>  (any (idInUsePure idN) <$> gets @FunClosures M.elems)
idInUsePure :: Int -> Stack -> Bool
idInUsePure idN stack = let stackIds = M.elems stack in idN `elem` stackIds
filterInUse :: Members [Reader Stack, State Heap, State FunClosures] r => Sem r ()
filterInUse =  
  put @Heap =<< M.traverseMaybeWithKey (\k v -> ifM (idInUse k) (pure (Just v)) (pure Nothing)) =<< get @Heap

mapFilterA :: (Applicative f, Ord k) => (a -> f Bool) -> M.Map k a -> f (M.Map k a) 
mapFilterA f = mapFilterWithKeyA (\_ x -> f x)
mapFilterKeyA f = mapFilterWithKeyA (\k _ -> f k)
mapFilterWithKeyA :: (Applicative f, Ord k) => (k -> a -> f Bool) -> M.Map k a -> f (M.Map k a)
mapFilterWithKeyA f m = do 
  let assocs = M.assocs m
  M.fromList <$> filterM (uncurry f) assocs
