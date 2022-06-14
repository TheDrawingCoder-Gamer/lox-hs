module Lox.Helpers where 

import Data.Map.Strict qualified as M
import Control.Monad (foldM)
import Data.Maybe (maybe)
import Polysemy
import Polysemy.State
import Control.Lens.Getter qualified as G
import Control.Lens.Setter qualified as S
import Control.Lens.Lens qualified as L
import Control.Lens.Prism qualified as P
import Control.Lens.Cons (_Cons, Cons)
import Control.Lens.Fold ((^?))
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

use :: Member (State s) r => G.Getting a s a -> Sem r a
use l = gets (G.view l)

assign :: Member (State s) r => S.ASetter s s a b -> b -> Sem r () 
assign l b = modify (S.set l b)
{-# INLINE assign #-}
modifying :: Member (State s) r => S.ASetter s s a b -> (a -> b) -> Sem r ()
modifying l f = modify (S.over l f)
{-# INLINE modifying #-}
prepending :: Member (State s) r => S.ASetter' s [a] -> a -> Sem r ()
prepending l b = modifying l (b :)

popping :: (Cons t t a a, Member (State s) r) => L.Lens' s t -> Sem r (Maybe a)
popping l = do 
  s <- (^? _Cons) <$> use l
  case s of 
    Nothing -> pure Nothing
    Just (h, t) -> do 
      assign l t
      pure (Just h)

{-# INLINE prepending #-}
infix 4 .=, %=, +=, -=, <>=

(.=) :: Member (State s) r => S.ASetter s s a b -> b -> Sem r ()
(.=) = assign
{-# INLINE (.=) #-}
(%=) :: Member (State s) r => S.ASetter s s a b -> (a -> b) -> Sem r ()
(%=) = modifying
{-# INLINE (%=) #-}
(+=) :: (Num a, Member (State s) r) => S.ASetter' s a -> a -> Sem r ()
l += b = l %= (+ b)
{-# INLINE (+=) #-}
(-=) :: (Num a, Member (State s) r) => S.ASetter' s a -> a -> Sem r ()
l -= b = l %= subtract b
{-# INLINE (-=) #-}
(<>=) :: (Semigroup a, Member (State s) r) => S.ASetter' s a -> a -> Sem r ()
l <>= b = l %= (<> b)
{-# INLINE (<>=) #-}
