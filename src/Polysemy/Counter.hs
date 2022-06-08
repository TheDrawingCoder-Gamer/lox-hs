{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Counter where

import Polysemy 
import Polysemy.Input
import Data.Maybe (fromJust)
data Counter m a where 
  MakeCount :: Counter m Int

makeSem ''Counter

runCounter :: forall (r ::EffectRow) a. Sem (Counter ': r) a -> Sem r a
runCounter = runInputList [0..] . reinterpret @Counter @(Input (Maybe Int)) (\case 
  MakeCount -> fromJust <$> input) 
