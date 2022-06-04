module Lox.NativeFun where

import Control.Monad ((=<<))
import Lox.Types
import Lox.Evaluate
import Lox.Helpers
import Data.Time.Clock.System
import Data.HashMap.Strict qualified as HM
import Polysemy.Final (embedFinal)
clock :: LoxValue
clock = LvNativeFun "clock" (LoxFunction $ \_ -> lxStmt . lxReturn . PLvNumber . fromIntegral . systemSeconds =<<  embedFinal getSystemTime) 0


lxReturn = LxReturn . LxLit
envWithNative :: LxEnv
envWithNative = emptyEnv { variables = HM.fromList [("clock", clock)] }
