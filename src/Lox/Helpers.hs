module Lox.Helpers where 

import Lox.Types
import Data.HashMap.Strict
emptyEnv = LxEnv HM.empty Nothing False
