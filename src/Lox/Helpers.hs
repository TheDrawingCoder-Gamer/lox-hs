module Lox.Helpers where 

import Lox.Types
import Data.HashMap.Strict qualified as HM
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
