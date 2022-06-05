{-# LANGUAGE TemplateHaskell #-}
module Polysemy.StackState where 

import Polysemy
import Polysemy.State qualified as S
import Data.Functor (($>))
import Data.List (uncons)
data StackState s m a where 
  Peek  :: StackState s m (Maybe s)
  Pop   :: StackState s m (Maybe s)
  Push  :: s -> StackState s m () 
  Get   :: StackState s m [s]
  Put   :: [s] -> StackState s m ()

makeSem ''StackState

peekN :: Member (StackState s) r => Int -> Sem r (Maybe s)
peekN n = do 
  s <- get
  if n < 0 || n >= length s then 
    pure Nothing
  else 
    pure . Just $ s !! n
poke  :: Member (StackState s) r => (s -> s) -> Sem r ()
poke f = do 
  s <- pop
  case s of 
    Just s' -> push (f s')
    Nothing -> pure ()

pokeN :: Member (StackState s) r => Int -> (s -> s) -> Sem r ()
pokeN n f = do 
  s <- get 
  if n < 0 || n >= length s then 
    pure () 
  else 
    let (xs, y:ys) = splitAt (n - 1) s in
    put $ xs ++ (f y:ys)
modify :: Member (StackState s) r => ([s] -> [s]) -> Sem r () 
modify f = do 
  s <- get
  put (f s)

stackStateToState :: forall (r :: EffectRow) s a. Sem (StackState s ': r) a -> Sem (S.State [s] ': r) a
stackStateToState = reinterpret (\case 
  Peek -> do 
    s <- S.get 
    if null s then pure Nothing else pure (Just (head s))
  Pop -> do 
    s <- S.get 
    case uncons s of 
      Nothing -> pure Nothing 
      Just (x,xs) -> S.put xs $> Just x
  Push x -> S.modify (x:) 
  Get -> S.get
  Put x -> S.put x
  )

