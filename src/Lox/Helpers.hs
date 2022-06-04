module Lox.Helpers where 

import Lox.Types
import Data.HashMap.Strict qualified as HM
emptyEnv = LxEnv HM.empty Nothing False
