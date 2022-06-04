{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
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
import Data.Hashable
import GHC.Generics (Generic)
type LoxParser = Parsec Text Text
instance ShowErrorComponent Text where 
  showErrorComponent = T.unpack
  errorComponentLen = T.length
data LoxValue
  = LvString Text
  | LvNumber Float
  | LvBool   Bool
  | LvNil
  | LvFun   LoxFun
  | LvClass LoxClass
  | LvInstance LoxClass (HM.HashMap Text LoxValue)
  deriving Show
data ParseLxValue 
  = PLvString Text
  | PLvNumber Float
  | PLvBool   Bool
  | PLvNil
  deriving (Show, Generic, Hashable)
data LoxClass = LoxClass Text (HM.HashMap Text FunInfo)
  deriving (Show, Generic, Hashable)
-- TODO: Come up with less stupid name
data LoxFun = LoxFun Text LoxFunction Int LxEnv
  deriving Show
instance Eq LoxValue where 
  (LvString s) == (LvString t) = s == t 
  (LvNumber x) == (LvNumber y) = x == y
  (LvBool   x) == (LvBool   y) = x == y
  LvNil == LvNil = True
  _ == _ = False
parseLitToValue (PLvString n) = LvString n
parseLitToValue (PLvNumber n) = LvNumber n
parseLitToValue (PLvBool   n) = LvBool   n
parseLitToValue PLvNil        = LvNil

prettyLoxValue (LvString n) = T.unpack n
prettyLoxValue (LvNumber n) = show n
prettyLoxValue (LvBool True) = "true"
prettyLoxValue (LvBool False) = "false"
prettyLoxValue LvNil         = "nil"
prettyLoxValue (LvFun (LoxFun name _ _ _)) = T.unpack $ "<fn " <> name <> ">"
prettyLoxValue (LvClass (LoxClass name _)) = T.unpack $ "class" <> name
prettyLoxValue (LvInstance (LoxClass name _) _) = T.unpack $ name <> "instance"
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
  deriving (Eq, Show, Generic, Hashable)
data LxUnopKind 
  = LxNot
  | LxNegate
  deriving (Eq, Show, Generic, Hashable)
data LxExpr 
  = LxBinop LxExpr LxExpr LxBinopKind
  | LxGroup LxExpr
  | LxLit   ParseLxValue
  | LxIdent Text
  | LxUnary Bool LxUnopKind LxExpr
  | LxCall  LxExpr [LxExpr]
  deriving (Show, Generic,  Hashable)
data LxStmt
  = LxExprStmt LxExpr
  | LxPrint    LxExpr
  | LxVar      Text   (Maybe LxExpr)
  | LxBlock    [LxStmt]
  | LxIf       LxExpr LxStmt (Maybe LxStmt)
  | LxWhile    LxExpr LxStmt
  | LxFunDecl  FunDecl
  | LxClassDecl Text (HM.HashMap Text FunInfo)
  | LxReturn   LxExpr
  deriving (Show, Generic, Hashable) 
data FunDecl = FunDecl Text FunInfo
  deriving (Show, Generic, Hashable)
data FunInfo = FunInfo [Text] [LxStmt]
  deriving (Show, Generic, Hashable)
data FunKind 
  = FunNormal
  | FunMethod
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
instance Hashable LoxFunction where 
  hashWithSalt _ _ = 0 -- LOL 
