module Lox.Helpers where 

import Data.Map.Strict qualified as M
import Control.Monad (foldM)
import Data.Maybe (maybe)
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

ifM b t f = do b <- b; if b then t else f

infixr 2 <||>
infixr 3 <&&>
(<||>) a = ifM a (pure True)
(<&&>) t f = ifM t f (pure False)

anyM p = foldr ((<||>) . p) (pure False)
andM p = foldr ((<&&>) . p) (pure True)

foldMWithKey :: Monad m => (a -> k -> b -> m a) -> a -> M.Map k b -> m a
foldMWithKey f z = foldM (\z' (kx, x) -> f z' kx x) z . M.toAscList

maybeM :: Monad m => m a -> m (Maybe a) -> m a
maybeM def act = maybe def pure =<< act

