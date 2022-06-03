module Lox.Types where 

import Text.Megaparsec hiding (State)
import Data.Void
import Data.Text (Text)
import Data.Text qualified as T
import Data.HashMap.Strict qualified as HM
import Polysemy
import Polysemy.State
import Polysemy.Fail
import Polysemy.Haskeline
import Polysemy.Error
type LoxParser = Parsec Text Text
instance ShowErrorComponent Text where 
  showErrorComponent = T.unpack
  errorComponentLen = T.length
data LoxValue
  = LvString Text
  | LvNumber Float
  | LvBool   Bool
  | LvNil
  | LvFun   LoxFunction Int LxEnv
  deriving Show
instance Eq LoxValue where 
  (LvString s) == (LvString t) = s == t 
  (LvNumber x) == (LvNumber y) = x == y
  (LvBool   x) == (LvBool   y) = x == y
  LvNil == LvNil = True
  _ == _ = False
prettyLoxValue (LvString n) = T.unpack n
prettyLoxValue (LvNumber n) = show n
prettyLoxValue (LvBool True) = "true"
prettyLoxValue (LvBool False) = "false"
prettyLoxValue LvNil         = "nil"
prettyLoxValue LvFun{}       = "Can't display functions"
data LxBinopKind 
  = LxEquals
  | LxUnequal
  | LxGreater
  | LxGreaterEq
  | LxLess
  | LxLessEq
  | LxPlus
  | LxMinus
  | LxTimes
  | LxDiv
  | LxAssign
  | LxAnd 
  | LxOr
  deriving (Eq, Show)
data LxUnopKind 
  = LxNot
  | LxNegate
  deriving (Eq, Show)
data LxExpr 
  = LxBinop LxExpr LxExpr LxBinopKind
  | LxGroup LxExpr
  | LxLit   LoxValue
  | LxIdent Text
  | LxUnary Bool LxUnopKind LxExpr
  | LxCall  LxExpr [LxExpr]
  deriving Show
data LxStmt
  = LxExprStmt LxExpr
  | LxPrint    LxExpr
  | LxVar      Text   (Maybe LxExpr)
  | LxBlock    [LxStmt]
  | LxIf       LxExpr LxStmt (Maybe LxStmt)
  | LxWhile    LxExpr LxStmt
  | LxFunDecl  Text   [Text] [LxStmt]
  | LxReturn   LxExpr
  deriving Show 
data FunKind 
  = FunNormal 
data LxEnv = LxEnv 
  { variables :: HM.HashMap Text LoxValue 
  , enclosing :: Maybe LxEnv
  , inFunction :: Bool }
  deriving Show
type ReturnState = (LxEnv, LoxValue)
newtype LoxFunction = LoxFunction (forall r. Members [State LxEnv, Fail, Error ReturnState, Haskeline, Embed IO] r => [LoxValue] -> Sem r ())
instance Show LoxFunction where
  -- LOL
  show _ = "<fn>"
