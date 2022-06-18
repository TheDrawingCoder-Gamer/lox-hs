{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Polysemy.ListBuild where

import Polysemy
import Polysemy.Writer
import Data.List (singleton)
import Data.Bifunctor qualified as BFu
data ListBuild i m a where
  LsBuildMore :: [i] -> ListBuild i m ()

makeSem ''ListBuild

lsBuild :: Member (ListBuild i) r => i -> Sem r ()
lsBuild = lsBuildMore . singleton
transformListBuildToWriter :: forall e1 e2 rInitial x i. ListBuild i (Sem rInitial) x -> Writer [i] (Sem rInitial) x
transformListBuildToWriter = \case 
  LsBuildMore items -> Tell items

lsBuildToWriter :: Member (Writer [i]) r => Sem (ListBuild i : r) a -> Sem r a
lsBuildToWriter = transform transformListBuildToWriter

lsBuildAsWriter :: forall (r :: EffectRow) a i. Sem (ListBuild i : r) a -> Sem (Writer [i] : r) a
lsBuildAsWriter = rewrite transformListBuildToWriter

runListBuild :: forall i (r :: EffectRow) a. Sem (ListBuild i : r) a -> Sem r ([i], a)
runListBuild = runWriterAssocR @[i] . lsBuildAsWriter

execListBuild :: forall i (r :: EffectRow) a. Sem (ListBuild i : r) a -> Sem r [i]
execListBuild = fmap fst . runListBuild


evalListBuild :: forall i (r :: EffectRow) a. Sem (ListBuild i : r) a -> Sem r a
evalListBuild = fmap snd . runListBuild
