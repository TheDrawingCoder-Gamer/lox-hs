{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Megaparsec where 

import Text.Megaparsec qualified as MP
import Data.Set qualified as S
import Polysemy
data Megaparsec e s m a where 
  ParseError :: MP.ParseError s e -> Megaparsec e s m a
  Label      :: String -> m a -> Megaparsec e s m a
  Try        :: m a -> Megaparsec e s m a
  LookAhead  :: m a -> Megaparsec e s m a
  NotFollowedBy :: m a -> Megaparsec e s m () 
  WithRecovery :: (MP.ParseError s e -> m a) -> m a -> Megaparsec e s m a
  Observing   :: m a -> Megaparsec e s m (Either (MP.ParseError s e) a)
  Eof    :: Megaparsec e s m () 
  Token :: (MP.Token s -> Maybe a) -> S.Set (MP.ErrorItem (MP.Token s)) -> Megaparsec e s m a
  Tokens :: (MP.Tokens s -> MP.Tokens s -> Bool) -> MP.Tokens s -> Megaparsec e s m (MP.Tokens s)
  TakeWhileP :: Maybe String -> (MP.Token s -> Bool) -> Megaparsec e s m (MP.Tokens s)
  TakeWhile1P :: Maybe String -> (MP.Token s -> Bool) -> Megaparsec e s m (MP.Tokens s)
  TakeP :: Maybe String -> Int -> Megaparsec e s m (MP.Tokens s)
  GetParserState :: Megaparsec e s m (MP.State s e) 
  UpdateParserState :: (MP.State s e -> MP.State s e) -> Megaparsec e s m ()

makeSem ''Megaparsec


