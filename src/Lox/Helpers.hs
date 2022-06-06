module Lox.Helpers where 

import Lox.Types
import Data.HashMap.Lazy qualified as HM
import Data.Text qualified as T
import Data.Coerce
emptyEnv = LxEnv HM.empty

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = 
  unsnoc' [] xs 
  where 
    unsnoc' :: [a] -> [a] -> Maybe ([a], a)
    unsnoc' ys [x] = 
      Just (reverse ys, x)
    unsnoc' ys (x:xs) = 
      unsnoc' (x:ys) xs
    unsnoc' _ [] = Nothing
snoc :: [a] -> a -> [a]
snoc a b = a ++ pure b

printEnvKeys :: [LxEnv] -> String
printEnvKeys = coerce ((show . map (map T.unpack . HM.keys)) :: [HM.HashMap T.Text LoxValue] -> String)
