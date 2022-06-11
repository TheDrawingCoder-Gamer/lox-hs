module Lox.Interp (interpret, execute, evaluate, showValue) where 

import Lox.Types
import Lox.Helpers
import Polysemy hiding (interpret)
import Polysemy.Error 
import Polysemy.Trace
import Polysemy.Reader
import Polysemy.State (get, put, gets, State, modify)
import Polysemy.Counter
import Data.Functor ((<&>), ($>))
import Text.Megaparsec (SourcePos, initialPos)
import Text.Read (readMaybe)
import Data.Maybe (maybe, fromMaybe)
import Data.Text qualified as T
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Control.Monad (void, foldM, filterM, (>=>))
-- rewrite. I am in great pain


interpret :: Members LxMembers r => Either [Stmt] Expr -> Sem r Stack
interpret (Left stmts) = execute stmts
interpret (Right expr) = ((evaluate expr >>= showValue ) >>= trace) *> ask

execute :: Members LxMembers r => [Stmt] -> Sem r Stack
execute [] = ask
execute (Eval expr:rs) = void (evaluate expr) *> execute rs
execute (Print expr:rs) = (trace =<< showValue =<< evaluate expr) *> execute rs
execute (VarDef name init:rs) = do 
  case init of 
    Just i -> do
      i' <- evaluate i
      defineVar name i' (execute rs)
    Nothing -> defineVar name LoxNil (execute rs)
execute (Block ss:rs) = execute ss  *> execute rs
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
  let fun = LoxFun (Just name) (length args) 0 args stmts False in
    defineFunction name fun (execute rs) 
execute (ClassDecl name superclass methods:rs) = do
  idN <- makeCount
  methods' <- foldMWithKey (\m name (FunInfo args ss) -> do 
    -- methods have same closure as class
    let fun = LoxFun (Just name)  (length args + 1) idN ("this":args) ss (name == "init")
    idN' <- makeCount
    modify @RefHeap (M.insert idN' fun)
    modify @Heap    (M.insert idN' (LoxRef idN'))
    pure $ M.insert name idN' m 
    ) M.empty methods
  superclassObj <- case superclass of 
    Nothing -> pure Nothing 
    Just (WithPos v p) -> do 
      daVar <- getVar v p
      case daVar of 
        LoxRef i -> do 
          daVal <- getFromRefHeap p i 
          case daVal of 
            LoxClass{} -> pure (Just i)
            _ -> runtimeError p "Can only inherit from classes"
        _ -> runtimeError p "Can only inherit from classes"
  let klass = LoxClass (LoxClassDef name superclassObj methods' idN) 
  stack <- asks @Stack (M.insert name idN)
  modify @RefHeap (M.insert idN klass)
  modify @Heap    (M.insert idN (LoxRef idN))
  let stack' = case superclassObj of
                Nothing -> stack
                Just i -> M.insert "super" i stack
  defineClosureWith stack' idN
  local (M.insert name idN) (execute rs)
execute (LReturn value:_) = throw . ReturnError =<< maybe (pure LoxNil) evaluate value
evaluate :: Members LxMembers r => Expr -> Sem r LoxValue
evaluate (Expr (Binop le re And) _) = evalBinopLazy le re $ \l re ->
  if valTruthy l then 
    evaluate re
  else 
    pure l
evaluate (Expr (Binop le re Or) _) = evalBinopLazy le re $ \l re -> 
  if valTruthy l then
    pure l 
  else
    evaluate re
evaluate (Expr (Binop le re Equals) _) = evalBinopStrict le re (\l r -> Right $ LoxBool (l == r)) 
evaluate (Expr (Binop le re Unequal) _) = evalBinopStrict le re (\l r -> Right $ LoxBool (l /= r))
evaluate (Expr (Binop le re Plus) pos) = evalBinopStrict le re (\l r -> 
  case (l, r) of 
    (LoxNumber ln, LoxNumber rn) -> Right $ LoxNumber (ln + rn)
    (LoxString ls, LoxString rs) -> Right $ LoxString (ls <> rs)
    _ -> Left $ InterpreterError pos "Mismatched types on plus operator")
evaluate (Expr (Binop le re Minus) _) = evalBinopNumOnly le re (-)
evaluate (Expr (Binop le re Times) _) = evalBinopNumOnly le re (*)
evaluate (Expr (Binop le re Divide) _) = evalBinopNumOnly le re (/)
evaluate (Expr (Binop le re Greater) _) = evalBinopNumGeneric le re (>) LoxBool
evaluate (Expr (Binop le re GreaterEq) _) = evalBinopNumGeneric le re (>=) LoxBool
evaluate (Expr (Binop le re Less) _) = evalBinopNumGeneric le re (<) LoxBool
evaluate (Expr (Binop le re LessEq) _) = evalBinopNumGeneric le re (<=) LoxBool
evaluate (Expr (Unary Not le) _) =  evaluate le <&> LoxBool . not . valTruthy
evaluate (Expr (Unary Negate le@(Expr{exprPos=pos})) _) =  evaluate le >>= fromEither @InterpError . valToNum pos <&> LoxNumber . negate
evaluate (Expr (Literal l) _) = pure l
evaluate (Expr (Grouping e) _) = evaluate e
evaluate (Expr (Call callee args) pos) = do 
  c <- evaluate callee
  as <- traverse evaluate args
  evalCall c as pos
evaluate (Expr (Identifier name) pos) =  getVar name pos
evaluate (Expr (Assign name expr) pos) = do 
  val <- evaluate expr
  assignVar pos name val
  pure val
evaluate (Expr (AnonFun params ss) _) = do 
  let fun = LoxFun Nothing (length params) 0 params ss False 
  idN <- makeCount
  defineClosure idN
  modify @RefHeap (M.insert idN (fun { funId = idN }))
  pure (LoxRef idN)
evaluate (Expr (Get expr name) pos) = do 
  obj <- evaluate expr
  getField obj name pos
evaluate (Expr (Set expr name val) pos) = do 
  obj <- evaluate expr
  idN <- makeFieldId obj name pos
  val' <- evaluate val 
  modify @Heap (M.insert idN val')
  pure val'
evaluate (Expr LThis pos) = getVar "this" pos
evaluate (Expr (LSuper name) pos) = do 
  this <- getVar "this" pos
  ref <- case this of 
    LoxRef i -> pure i
    _ -> unexpected
  super <- getVar "super" pos
  superObj <- case super of 
                LoxRef i -> getFromRefHeap pos i
                _ -> unexpectedPos pos
  findMethod pos superObj name ref
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
evalBinopNum le@(Expr _ lpos) re@(Expr _ rpos) f = do 
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
evalCall (LoxRef i) args pos = do 
  val <- getFromRefHeap pos i
  evalCallRef val Nothing args pos
evalCall (LoxMethodRef _ boundTo i) args pos = do
  val <- getFromRefHeap pos i 
  evalCallRef val (Just boundTo) args pos
evalCall _ _ pos = runtimeError pos "Can only call functions"
evalCallRef :: Members LxMembers r => LoxRefValue -> Maybe Int -> [LoxValue] -> SourcePos -> Sem r LoxValue
evalCallRef (LoxFun name arity idN params stmts isInit) bindedTo args pos = do
  -- even tho it's a Maybe, we still should fail with unexpected of it's not found
  let boundTo = LoxRef <$> bindedTo
  args' <- case boundTo of { Nothing -> pure args; Just v -> pure (v:args) }
  checkArity pos arity (length args')
  closure <- gets @FunClosures (M.lookup idN)
  case closure of 
    Nothing -> runtimeError pos "Couldn't find closure for function"
    Just cls -> (do
      newStack <- ask >>= defineArgs params args'
      runReader (M.union cls newStack) (execute stmts) $> LoxNil) 
      `catch` 
      \case 
        ReturnError value -> if isInit then maybeM unexpected (pure boundTo) else pure value 
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
evalCallRef (LoxNativeFun _ fun arity) _ args pos = do 
  checkArity pos arity (length args) 
  fun args
evalCallRef (LoxClass klassDef@(LoxClassDef name _ methods idN)) _ args pos = do
  i <- makeCount
  let inst = LoxInstance name idN M.empty i
  modify @Heap (M.insert i (LoxRef i))  
  modify @RefHeap (M.insert i inst)
  case M.lookup "init" methods of 
    Just meth -> do 
      val <- getFromHeap pos meth
      case val of 
        LoxRef ref -> do
          daMeth <- getFromRefHeap pos meth
          evalCallRef daMeth (Just i) args pos
          pure (LoxRef i)
        _ -> unexpected
    Nothing -> do
      if null args then 
        pure (LoxRef i)
      else 
        arityError pos 0 (length args)
evalCallRef _ _ _ pos = runtimeError pos "Can only call functions"

checkArity :: Member (Error InterpError) r => SourcePos -> Int -> Int -> Sem r ()
checkArity pos expected actual = 
  if expected == actual then 
    pure ()
  else 
    arityError pos expected actual
getField :: Members LxMembers r => LoxValue -> T.Text -> SourcePos -> Sem r LoxValue
getField (LoxRef i) name pos = do
  val <- getFromRefHeap pos i
  refGetField val name pos
getField _ _ pos = runtimeError pos "Can only access fields of a class instance"

refGetField :: Members LxMembers r => LoxRefValue -> T.Text -> SourcePos -> Sem r LoxValue
refGetField (LoxInstance _ classRef fields idN) name pos =
  case M.lookup name fields of
    Nothing -> do
      loxClass <- getFromRefHeap pos classRef
      findMethod pos loxClass name idN
    Just v -> 
      getFromHeap pos v
refGetField _ _ pos = runtimeError pos "Can only access fields of a class instance"

makeFieldId :: Members LxMembers r => LoxValue -> T.Text -> SourcePos -> Sem r Int
makeFieldId (LoxRef i) name pos = do 
  val <- getFromRefHeap pos i
  refMakeFieldId val name pos
makeFieldId _ _ pos = runtimeError pos "Can only access fields of a class instance"

refMakeFieldId :: Members LxMembers r => LoxRefValue -> T.Text -> SourcePos -> Sem r Int
refMakeFieldId inst@(LoxInstance _ _ fields idN) name pos = 
  case M.lookup name fields of 
    Nothing -> do
      newIdN <- makeCount
      modify @Heap (M.insert newIdN LoxNil)
      modify @RefHeap (M.insert idN (inst { instFields = M.insert name newIdN fields }))
      pure newIdN
    Just v -> pure v
refMakeFieldId _ _ pos = runtimeError pos "Can only access fields of a class instance"

findMethod :: Members LxMembers r => SourcePos -> LoxRefValue -> T.Text -> Int -> Sem r LoxValue
findMethod pos (LoxClass (LoxClassDef _ superclassRef methods _)) name bindTo =
  case M.lookup name methods of 
    Nothing ->  
      case superclassRef of 
        Nothing -> runtimeError pos ("Undefined property '" ++ T.unpack name ++ "'") 
        Just ref -> do
          superclass <- getFromRefHeap pos ref
          findMethod pos superclass name bindTo
    Just m -> do
      obj <- getFromHeap pos m
      case obj of 
        LoxRef i -> 
          pure $ LoxMethodRef name bindTo i
        _ -> unexpected
findMethod pos _ _ _ = unexpectedPos pos
runtimeError :: Member (Error InterpError) r => SourcePos -> String -> Sem r a
runtimeError p s = throw @InterpError $ InterpreterError p s

unexpected :: Member (Error InterpError) r => Sem r a
unexpected = throw Unexpected

unexpectedPos :: Member (Error InterpError) r => SourcePos -> Sem r a
unexpectedPos = throw . UnexpectedPos
conversionError :: Member (Error InterpError) r => SourcePos -> String -> String -> Sem r a
conversionError pos from = throw . conversionErrorValue pos from

undefinedVarError :: Member (Error InterpError) r => SourcePos -> String -> Sem r a
undefinedVarError pos = throw . InterpreterError pos . ("Undefined variable " ++)
conversionErrorValue :: SourcePos -> String -> String -> InterpError
conversionErrorValue pos from to = InterpreterError pos ("Can't convert " ++ from ++ " to " ++ to)

arityError :: Member (Error InterpError) r => SourcePos -> Int -> Int -> Sem r a
arityError pos expected got = throw $ InterpreterError pos ("Expected " ++ show expected ++ " args, got " ++ show got)
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
valToNum pos (LoxRef{}) = Left (conversionErrorValue pos "reference" "number")
valToNum pos (LoxMethodRef{}) = Left (conversionErrorValue pos "reference" "number")

getVar :: Members LxMembers r => T.Text -> SourcePos -> Sem r LoxValue
getVar name pos = do 
  stack <- ask
  heap <- get
  case M.lookup name stack of
    Just i -> 
      maybe unexpected pure (M.lookup i heap)
    Nothing ->
      undefinedVarError pos (T.unpack name)
cloneId :: Members LxMembers r => Int -> Sem r Int
cloneId idN = do 
  heap <- get
  val <- maybe unexpected pure (M.lookup idN heap) 
  newIdn <- makeCount
  modify @Heap (M.insert newIdn val)
  pure newIdn

defineVar :: Members LxMembers r => T.Text -> LoxValue -> Sem r a -> Sem r a 
defineVar k v sem =  do
  idN <- makeCount
  modify (M.insert idN v)
  local (M.insert k idN) sem

defineFunction :: Members LxMembers r => T.Text -> LoxRefValue -> Sem r a -> Sem r a
defineFunction k fn@(LoxFun{}) sem = do 
  idN <- makeCount
  modify @Heap (M.insert idN (LoxRef idN) )
  modify @RefHeap (M.insert idN (fn { funId = idN }))
  stack <- asks (M.insert k idN)
  defineClosureWith stack idN
  local @Stack (M.insert k idN) sem
defineFunction _ _ _= unexpected
defineClosureWith :: Members LxMembers r => Stack -> Int -> Sem r ()
defineClosureWith stack idN = do 
  modify @FunClosures (M.insert idN stack)
defineClosure :: Members LxMembers r => Int -> Sem r ()
defineClosure idN = do 
  stack <- ask @Stack
  defineClosureWith stack idN

assignVar :: Members LxMembers r => SourcePos -> T.Text -> LoxValue -> Sem r () 
assignVar pos k v = do 
  idN <- asks (M.lookup k)
  case idN of 
    Just idN' -> 
      modify (M.insert idN' v) 
    Nothing -> 
      undefinedVarError pos (T.unpack k)

varInUse :: Members LxMembers r => T.Text -> Sem r Bool
varInUse name = do 
  stack <- ask @Stack 
  if M.member name stack then 
    pure True 
  else do 
    let stackValues = S.fromList (M.elems stack)
    closures <- gets @FunClosures (M.elems . flip M.restrictKeys stackValues)
    pure $ any (M.member name) closures 

idInUse :: Members LxMembers r => Int -> Sem r Bool
idInUse idN = do 
  stackIds <- asks @Stack M.elems
  pure (idN `elem` stackIds) <||>  (any (uncurry (idInUsePure idN)) <$> gets @FunClosures M.assocs)
idInUsePure :: Int -> Int -> Stack -> Bool
idInUsePure idN stackIdn stack = 
  let 
    values = M.elems stack
  in
    (stackIdn /= idN && idN `elem` values) 
filterInUse :: Members LxMembers r => Sem r ()
filterInUse = do 
  -- TODO: classes break this
  put @FunClosures =<< M.traverseMaybeWithKey (\k v -> ifM (idInUse k) (pure (Just v)) (pure Nothing)) =<< get @FunClosures
  put @Heap =<< M.traverseMaybeWithKey (\k v -> ifM (idInUse k) (pure (Just v)) (pure Nothing)) =<< get @Heap
mapFilterA :: (Applicative f, Ord k) => (a -> f Bool) -> M.Map k a -> f (M.Map k a) 
mapFilterA f = mapFilterWithKeyA (\_ x -> f x)
mapFilterKeyA f = mapFilterWithKeyA (\k _ -> f k)
mapFilterWithKeyA :: (Applicative f, Ord k) => (k -> a -> f Bool) -> M.Map k a -> f (M.Map k a)
mapFilterWithKeyA f m = do 
  let assocs = M.assocs m
  M.fromList <$> filterM (uncurry f) assocs

getFromRefHeap :: Members LxMembers r => SourcePos -> Int -> Sem r LoxRefValue
getFromRefHeap pos i = do
  val <- maybeGetFromRefHeap i
  case val of
    Nothing -> unexpectedPos pos
    Just v -> pure v


getFromHeap :: Members LxMembers r => SourcePos -> Int -> Sem r LoxValue
getFromHeap pos i =
  maybeM (unexpectedPos pos) (maybeGetFromHeap i)
getFromHeap_ :: Members LxMembers r => Int -> Sem r LoxValue
getFromHeap_ i = 
  maybe unexpected pure =<< maybeGetFromHeap i
getFromRefHeap_ :: Members LxMembers r => Int -> Sem r LoxRefValue
getFromRefHeap_ i = maybeM unexpected (maybeGetFromRefHeap i)
maybeGetFromRefHeap :: Member (State RefHeap) r => Int -> Sem r (Maybe LoxRefValue)
maybeGetFromRefHeap i = gets @RefHeap (M.lookup i)

maybeGetFromHeap    :: Member (State Heap   ) r => Int -> Sem r (Maybe LoxValue)
maybeGetFromHeap    i = gets @Heap    (M.lookup i)

showValue :: Members LxMembers r => LoxValue -> Sem r String
showValue (LoxRef i) = do 
  val <- getFromRefHeap_ i 
  pure (prettyLoxRefValue val)
showValue i = pure (prettyLoxValue i)
