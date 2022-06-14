{-# LANGUAGE PatternSynonyms, StandaloneDeriving #-}
module Lox.Types where 

import Text.Megaparsec hiding (State)
import Data.Void
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict   qualified as M
import Polysemy
import Polysemy.State
import Polysemy.Fail
import Polysemy.Haskeline
import Polysemy.Error
import Polysemy.Fixpoint
import Polysemy.Counter
import Polysemy.Reader
import Polysemy.Trace (Trace)
type LoxParser = Parsec Text Text
instance ShowErrorComponent Text where 
  showErrorComponent = T.unpack
  errorComponentLen = T.length
data InterpError
  = Unexpected
  | UnexpectedPos SourcePos
  | InterpreterError SourcePos String -- string to aid in throwing
  | ReturnError LoxValue

showInterpError Unexpected = "something went wrong : (" 
showInterpError (UnexpectedPos pos) = "Internal error at " ++ sourcePosPretty pos
showInterpError (InterpreterError pos err) = "Error at " ++ sourcePosPretty pos ++ ": " ++ err
showInterpError (ReturnError{}) = "uncaught return - perhaps you returned at top level?"

data LoxValue 
  = LoxString Text
  | LoxNumber Double
  | LoxBool   Bool
  | LoxNil 
  | LoxRef Int
  | LoxMethodRef 
    { funRefName    :: Text
    , funRefBoundTo :: Int
    , funRefRef     :: Int }
  deriving (Eq, Ord, Show)
data LoxRefValue 
  = LoxClass LoxClassDef
  | LoxInstance 
    { instName :: Text
    , instClassRef :: Int
    , instFields :: M.Map Text Int
    , instIdN :: Int}
  | LoxFun    
    { funName :: Maybe Text 
    , funArity :: Int
    , funId   :: Int
    , funArgs :: [Text]
    , funStmt :: [Stmt] 
    , funInit :: Bool}
  | LoxNativeFun Text (forall (r :: EffectRow). Members LxMembers r => [LoxValue] -> Sem r LoxValue) Int
data LoxClassDef = LoxClassDef Text (Maybe Int) (M.Map Text Int) Int
  deriving (Eq, Ord, Show)
data Expr = Expr 
  { exprNode :: ExprNode 
  , exprPos  :: SourcePos }
  deriving (Eq, Ord, Show)

data BinopKind 
  = Equals 
  | Unequal
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Plus
  | Minus
  | Times
  | Divide
  | And
  | Or
  deriving (Eq, Ord, Show)

data UnaryKind 
  = Not
  | Negate
  deriving (Eq, Ord, Show)

data ExprNode 
  = Binop Expr Expr BinopKind
  | Unary UnaryKind Expr
  | Literal LoxValue
  | Assign Text Expr
  | Get Expr Text
  | Set Expr Text Expr
  | Identifier Text
  | Call Expr [Expr]
  | Grouping Expr
  | AnonFun [Text] [Stmt]
  | LThis
  | LSuper Text
  deriving (Eq, Ord, Show)

data Stmt = Stmt
  { stmtNode :: StmtNode
  , stmtPos :: SourcePos} 
  deriving (Eq, Ord, Show)
data StmtNode 
  = Eval Expr
  | Print Expr
  | VarDef Text (Maybe Expr)
  | Block  [Stmt]
  | LIf Expr Stmt (Maybe Stmt)
  | LWhile Expr Stmt
  | LFunDecl FunDecl
  | ClassDecl Text (Maybe (WithPos Text)) (M.Map Text FunInfo)
  | LReturn (Maybe Expr)
  deriving (Eq, Ord, Show)

data WithPos a = WithPos a SourcePos

deriving instance Show a => Show (WithPos a)
deriving instance Eq a => Eq (WithPos a)
deriving instance Ord a => Ord (WithPos a)
prettyLoxValue (LoxString n) = T.unpack n
prettyLoxValue (LoxNumber n) = show n
prettyLoxValue (LoxBool True) = "true"
prettyLoxValue (LoxBool False) = "false"
prettyLoxValue LoxNil         = "nil"
prettyLoxValue (LoxRef i) = "<ptr " ++ show i ++ ">"
prettyLoxValue (LoxMethodRef name _ _) = T.unpack $ "<method " <> name <> ">"
prettyLoxRefValue (LoxFun {funName=Just name}) = T.unpack $ "<fn " <> name <> ">"
prettyLoxRefValue (LoxFun{}) = "<fn>"
prettyLoxRefValue (LoxClass (LoxClassDef name _ _ _)) = T.unpack $ "class " <> name
prettyLoxRefValue (LoxInstance name _ _ _) = T.unpack $ name <> " instance"
prettyLoxRefValue (LoxNativeFun name _ _) = T.unpack name
data FunDecl = FunDecl Text FunInfo
  deriving (Eq, Ord, Show)
data FunInfo = FunInfo [Text] [Stmt]
  deriving (Eq, Ord, Show)
data ResLoc 
  = LocNormal
  | LocFuntion
data FunKind 
  = FunNormal
  | FunMethod
  deriving (Eq, Ord, Show)
data EvalState = EvalState
  { evalEnv :: Heap
  , evalLocals :: M.Map Expr Int }
--data EvalError 
--  = EvalTypeError String

type LxMembers = [Reader Stack, State Heap, State RefHeap, State FunClosures, Counter, Error InterpError, Fixpoint, Trace, Final IO]
{-# COMPLETE FunDecl' #-}
{-# COMPLETE Eval, Print, VarDef, Block, LIf, LWhile, ClassDecl, LReturn, LFunDecl' #-}
pattern LFunDecl' name args stmts = LFunDecl (FunDecl' name args stmts)
pattern FunDecl' name args stmts = FunDecl name (FunInfo args stmts)


type Heap = M.Map Int LoxValue
type RefHeap = M.Map Int LoxRefValue
type Stack = M.Map T.Text Int
type FunClosures = M.Map Int Stack

