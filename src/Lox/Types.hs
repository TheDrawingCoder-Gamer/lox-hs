{-# LANGUAGE PatternSynonyms #-}
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
  | InterpreterError SourcePos String -- string to aid in throwing
  | ReturnError LoxValue 
data LoxValue 
  = LoxString Text
  | LoxNumber Double
  | LoxBool   Bool
  | LoxNil 
  | LoxFun    
    { funName :: Text 
    , funArity :: Int
    , funId   :: Int
    , funArgs :: [Text]
    , funStmt :: [Stmt] }
  | LoxClass  LoxClassDef
  | LoxInstance Int LoxClassDef
  deriving (Eq, Ord, Show)

data LoxClassDef = LoxClassDef Text (Maybe LoxValue) (M.Map Text LoxValue)
  deriving (Eq, Ord, Show)
data Expr = Expr 
  { exprNode :: ExprNode 
  , exprPos  :: SourcePos 
  , exprId   :: Int }
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
  | Identifier Text
  | Call Expr [Expr]
  | Grouping Expr
  | LxParseFail T.Text -- I hate this
  deriving (Eq, Ord, Show)

data Stmt 
  = Eval Expr
  | Print Expr
  | VarDef Text (Maybe Expr)
  | Block  [Stmt]
  | LIf Expr Stmt (Maybe Stmt)
  | LWhile Expr Stmt
  | LFunDecl FunDecl
  | ClassDecl Text (M.Map Text FunInfo)
  | LReturn (Maybe Expr)
  | HeapDump
  | StackDump
  | ClosuresDump
  deriving (Eq, Ord, Show)
prettyLoxValue (LoxString n) = T.unpack n
prettyLoxValue (LoxNumber n) = show n
prettyLoxValue (LoxBool True) = "true"
prettyLoxValue (LoxBool False) = "false"
prettyLoxValue LoxNil         = "nil"
prettyLoxValue (LoxFun {funName=name}) = T.unpack $ "<fn " <> name <> ">"
prettyLoxValue (LoxClass (LoxClassDef name _ _)) = T.unpack $ "class" <> name
prettyLoxValue (LoxInstance _ (LoxClassDef name _ _)) = T.unpack $ name <> "instance"
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
data EvalState = EvalState
  { evalEnv :: Heap
  , evalLocals :: M.Map Expr Int }
--data EvalError 
--  = EvalTypeError String

type LxMembers = [Reader Stack, State Heap, State FunClosures, Counter, Error InterpError, Fixpoint, Trace, Final IO]
{-# COMPLETE FunDecl' #-}
pattern LFunDecl' name args stmts = LFunDecl (FunDecl' name args stmts)
pattern FunDecl' name args stmts = FunDecl name (FunInfo args stmts)


type Heap = M.Map Int LoxValue
type Stack = M.Map T.Text Int
type FunClosures = M.Map Int Stack

