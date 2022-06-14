{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Polysemy.Build where

import Data.Binary qualified as B
import Data.Binary.Put qualified as P
import Polysemy
import Polysemy.Writer
import Control.Monad.ST
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Bifunctor qualified as BFu

data Build m a where 
  BuildRaw :: BSB.Builder -> Build m ()

makeSem ''Build

build :: (Member Build r, B.Binary s) => s -> Sem r ()
build = buildBin . B.put

buildBin :: Member Build r => P.Put -> Sem r () 
buildBin = buildRaw . P.execPut
buildToWriter :: Member (Writer BSB.Builder) r => Sem (Build : r) a -> Sem r a
buildToWriter = interpret $ \case 
  BuildRaw s -> tell s 

buildAsWriter :: forall (r :: EffectRow) a. Sem (Build : r) a -> Sem (Writer BSB.Builder : r) a 
buildAsWriter = rewrite $ \case 
  BuildRaw s -> Tell s

runBuild :: forall a (r :: EffectRow). Sem (Build : r) a -> Sem r (BS.ByteString, a)
-- the quest for point free
runBuild = fmap @(Sem r) (BFu.first @(,) BSB.toLazyByteString) . (runWriter @BSB.Builder . buildAsWriter)

evalBuild :: forall a (r :: EffectRow). Sem (Build : r) a -> Sem r a
evalBuild = fmap snd . runBuild

execBuild :: forall a (r :: EffectRow). Sem (Build : r) a -> Sem r BS.ByteString
execBuild = fmap fst . runBuild
