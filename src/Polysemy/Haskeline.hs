{-# LANGUAGE TemplateHaskell, UndecidableInstances, LambdaCase #-}
module Polysemy.Haskeline where 

import Polysemy
import Polysemy.Embed
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.IO qualified as H
import System.Console.Haskeline.Completion (noCompletion)
import Control.Monad.Catch
data Haskeline m a where 
  GetInputLine :: String ->  Haskeline m (Maybe String)
  GetInputLineWithInitial :: String -> (String, String) -> Haskeline m (Maybe String)
  GetInputChar :: String -> Haskeline m (Maybe Char)
  GetPassword :: Maybe Char -> String -> Haskeline m (Maybe String)
  WaitForAnyKey :: String -> Haskeline m Bool
  OutputStr :: String -> Haskeline m ()
  OutputStrLn :: String -> Haskeline m () 
  GetExternalPrint :: Haskeline m (String -> IO ())
makeSem ''Haskeline

haskelineToInputState :: Member (Embed IO) r => H.InputState -> Sem (Haskeline ': r) a -> Sem r a
haskelineToInputState state = do 
  interpret $ \case
    GetInputLine prompt -> embed $ H.queryInput state (H.getInputLine prompt)
    GetInputLineWithInitial prompt init -> embed $ H.queryInput state (H.getInputLineWithInitial prompt init)
    GetInputChar prompt -> embed $ H.queryInput state (H.getInputChar prompt)
    GetPassword mask prompt -> embed $ H.queryInput state (H.getPassword mask prompt)
    WaitForAnyKey prompt -> embed $ H.queryInput state (H.waitForAnyKey prompt)
    OutputStr out -> embed $ H.queryInput state (H.outputStr out)
    OutputStrLn out -> embed $ H.queryInput state (H.outputStrLn out)
    GetExternalPrint -> embed $ H.queryInput state H.getExternalPrint

haskelineToIOSettings :: Member (Embed IO) r => H.Settings IO -> Sem (Haskeline ': r) a -> Sem r a
haskelineToIOSettings conf inp = do 
  state <- embed $ H.initializeInput conf
  daRes <- haskelineToInputState state inp
  embed $ H.closeInput state
  pure daRes
haskelineToIO :: Member (Embed IO) r => Sem (Haskeline ': r) a -> Sem r a 
haskelineToIO = haskelineToIOSettings (H.setComplete noCompletion H.defaultSettings)
