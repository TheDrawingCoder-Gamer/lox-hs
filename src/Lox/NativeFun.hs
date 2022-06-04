module Lox.NativeFun where

import Control.Monad ((=<<))
import Lox.Types
import Lox.Evaluate
import Lox.Helpers
import Data.Time.Clock.System
import Data.HashMap.Strict qualified as HM
import Polysemy.Embed (embed)
clock :: LoxValue
clock = LvFun (LoxFun "clock" (LoxFunction $ \_ -> lxRun . Left . pure . lxReturn . PLvNumber . fromIntegral . systemSeconds =<<  embed getSystemTime) 0 emptyEnv )

lxReturn = LxReturn . LxLit
envWithNative :: LxEnv
envWithNative = emptyEnv { variables = HM.fromList [("clock", clock)] }
