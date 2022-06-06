module Lox.NativeFun where

import Control.Monad ((=<<))
import Lox.Types
import Lox.Evaluate
import Lox.Helpers
import Data.Time.Clock.System
import Data.HashMap.Lazy qualified as HM
import Polysemy.Final (embedFinal)
clock :: LoxValue
clock = LvNativeFun $ LoxNativeFun "clock" (\_ -> LvNumber . fromIntegral . systemSeconds <$>  embedFinal getSystemTime) 0


lxReturn = LxReturn . Just . LxLit
envWithNative :: LxEnv
envWithNative = emptyEnv { variables = HM.fromList [("clock", clock)] }
