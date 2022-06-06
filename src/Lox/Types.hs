{-# LANGUAGE DeriveGeneric, DeriveAnyClass, PatternSynonyms #-}
module Lox.Types where 

import Text.Megaparsec hiding (State)
import Data.Void
import Data.Text (Text)
import Data.Text qualified as T
-- lazy because of closures
import Data.HashMap.Lazy qualified as HM
import Polysemy
import Polysemy.StackState
import Polysemy.Fail
import Polysemy.Haskeline
import Polysemy.Error
import Polysemy.Fixpoint
import Data.Hashable
import Polysemy.Trace (Trace)
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
  | LvFun   { lvFunction :: FunDecl, lvClosure :: [LxEnv] }
  -- preferably, I would use an actual haskell function for native functions
  | LvNativeFun {nativeFunction :: LoxNativeFun}
  | LvClass LoxClass
  | LvInstance LoxClass (HM.HashMap Text LoxValue)

data ParseLxValue 
  = PLvString Text
  | PLvNumber Float
  | PLvBool   Bool
  | PLvNil
  deriving (Show, Generic, Hashable)
data LoxClass = LoxClass Text (HM.HashMap Text FunInfo)
  deriving (Show, Generic, Hashable)
data LoxNativeFun = LoxNativeFun 
  { lvName :: Text 
  , lvFun :: NativeFunDecl
  , lvArity :: Int }
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
prettyLoxValue (LvFun (FunDecl' name _ _) _) = T.unpack $ "<fn " <> name <> ">"
prettyLoxValue (LvClass (LoxClass name _)) = T.unpack $ "class" <> name
prettyLoxValue (LvInstance (LoxClass name _) _) = T.unpack $ name <> "instance"
prettyLoxValue (LvNativeFun (LoxNativeFun{lvName=name})) = T.unpack $ "<fn " <> name <> ">"
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
  | LxIdent IdentInfo 
  | LxEAssign IdentInfo LxExpr
  | LxUnary Bool LxUnopKind LxExpr
  | LxCall  LxExpr [LxExpr]
  deriving (Show, Generic,  Hashable)
data IdentInfo = IdentInfo 
  { identName :: Text
  , identDepth :: Maybe Int}
  deriving (Show, Generic, Hashable)
data LxStmt
  = LxExprStmt LxExpr
  | LxPrint    LxExpr
  | LxVar      Text   (Maybe LxExpr)
  | LxBlock    [LxStmt]
  | LxIf       LxExpr LxStmt (Maybe LxStmt)
  | LxWhile    LxExpr LxStmt
  | LxFunDecl  FunDecl
  | LxClassDecl Text (HM.HashMap Text FunInfo)
  | LxReturn   (Maybe LxExpr)
  deriving (Show, Generic, Hashable) 
data FunDecl = FunDecl Text FunInfo
  deriving (Show, Generic, Hashable)
data FunInfo = FunInfo [Text] [LxStmt]
  deriving (Show, Generic, Hashable)
data ResLoc 
  = LocNormal
  | LocFuntion
data FunKind 
  = FunNormal
  | FunMethod
newtype LxEnv = LxEnv 
  { variables :: HM.HashMap Text LoxValue }
type ReturnState = ([LxEnv], LoxValue)
type NativeFunDecl = forall r. Members LxMembers r => [LoxValue] -> Sem r LoxValue
type LxMembers = [Fail, StackState LxEnv, Error ReturnState, Fixpoint, Trace, Final IO]
{-# COMPLETE FunDecl' #-}
{-# COMPLETE IdentInfo_ #-}
{-# COMPLETE LxIdent_, LxBinop, LxGroup, LxLit, LxEAssign, LxUnary, LxCall #-}
{-# COMPLETE LxIdent', LxBinop, LxGroup, LxLit, LxEAssign, LxUnary, LxCall #-}
pattern LxFunDecl' name args stmts = LxFunDecl (FunDecl' name args stmts)
pattern FunDecl' name args stmts = FunDecl name (FunInfo args stmts)
pattern IdentInfo_ name    <- IdentInfo name _ where
  IdentInfo_ name = IdentInfo name Nothing
pattern LxIdent_ name = LxIdent (IdentInfo_ name)
pattern LxIdent' name scope = LxIdent (IdentInfo name scope)
