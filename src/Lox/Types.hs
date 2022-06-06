{-# LANGUAGE DeriveGeneric, DeriveAnyClass, PatternSynonyms, StandaloneDeriving, DerivingVia #-}
module Lox.Types where 

import Text.Megaparsec hiding (State)
import Data.Void
import Data.Text (Text)
import Data.Text qualified as T
-- lazy because of closures
import Data.HashMap.Lazy qualified as HM
import Polysemy
import Polysemy.State
import Polysemy.Fail
import Polysemy.Haskeline
import Polysemy.Error
import Polysemy.Fixpoint
import Data.Hashable
import Polysemy.Trace (Trace)
import GHC.Generics (Generic)
import Data.IORef
import Data.Unique
type LoxParser = Parsec Text Text
instance ShowErrorComponent Text where 
  showErrorComponent = T.unpack
  errorComponentLen = T.length
data LoxValue
  = LvString Text
  | LvNumber Float
  | LvBool   Bool
  | LvNil
  | LvFun   { lvFunction :: FunDecl, lvClosure :: IORef LxEnv }
  -- preferably, I would use an actual haskell function for native functions
  | LvNativeFun {nativeFunction :: LoxNativeFun}
  | LvClass LoxClass
  | LvInstance LoxClass (HM.HashMap Text (IORef LoxValue))

data ParseLxValue 
  = PLvString Text
  | PLvNumber Float
  | PLvBool   Bool
  | PLvNil
  deriving (Eq, Show, Generic, Hashable)
data LoxClass = LoxClass Text (HM.HashMap Text FunInfo)
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
data LxExpr = LxExpr 
  { exprNode :: LxExprNode 
  , exprUniq :: Unique 
  , exprPos  :: SourcePos }
  deriving (Eq, Generic, Hashable)
data IdentInfo = IdentInfo
  { identUniq :: Unique
  , identPos  :: SourcePos }
data LxExprNode
  = LxBinop LxExpr LxExpr LxBinopKind
  | LxGroup LxExpr
  | LxLit   ParseLxValue
  | LxIdent Text 
  | LxEAssign LxExpr LxExpr
  | LxUnary Bool LxUnopKind LxExpr
  | LxCall  LxExpr [LxExpr]
  deriving (Eq, Generic, Hashable)
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
data FunDecl = FunDecl Text FunInfo
data FunInfo = FunInfo [Text] [LxStmt]
data ResLoc 
  = LocNormal
  | LocFuntion
data FunKind 
  = FunNormal
  | FunMethod
data LxEnv = LxEnv 
  { variables :: HM.HashMap Text (IORef LoxValue)
  , enclosing :: Maybe (IORef LxEnv)}
type ReturnState = ([LxEnv], LoxValue)
data EvalState = EvalState
  { evalEnv :: LxEnv
  , evalLocals :: HM.HashMap LxExpr Int }
--data EvalError 
--  = EvalTypeError String
type NativeFunDecl = forall r. Members LxMembers r => [LoxValue] -> Sem r LoxValue
type LxMembers = [Fail, State EvalState, Error ReturnState, Fixpoint, Trace, Final IO]
{-# COMPLETE FunDecl' #-}
pattern LxFunDecl' name args stmts = LxFunDecl (FunDecl' name args stmts)
pattern FunDecl' name args stmts = FunDecl name (FunInfo args stmts)

newtype NoHash a = NoHash a
instance Hashable (NoHash a) where 
  hashWithSalt salt _ = 0
-- I LOVE ORPHANS
instance Hashable Pos where 
  hashWithSalt salt p = 
    let p' = unPos p in 
      hashWithSalt salt p'

deriving instance Hashable SourcePos

