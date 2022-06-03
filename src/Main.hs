module Main where

import Lox.Parser 
import Lox.Evaluate
import Lox.Types
import Lox.Helpers
import Text.Megaparsec
import Data.Either (fromRight)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Polysemy.Haskeline
import Polysemy
import Polysemy.Embed
import Polysemy.State qualified as PS
import Polysemy.Fail
import Polysemy.Error
import Data.HashMap.Strict qualified as HM
import Data.List (stripPrefix)
import Control.Monad (void)
import Lox.NativeFun
import Data.Char (isSeparator)
import Data.Bifunctor qualified as BFu

main :: IO ()
main = void $ runM $ runError . PS.evalState (T.empty, False) . PS.evalState envWithNative $ haskelineToIO loop
loop :: Sem [Haskeline, PS.State LxEnv, PS.State (T.Text, Bool), Error ReturnState, Embed IO] () 
loop = do 
  (txt, active) <- PS.get @(T.Text, Bool)
  line <- getInputLine $ if active then ">>: " else ">>> " 
  
  case line of 
    Nothing -> 
      pure () -- STOP
    Just ln ->
    
      case stripPrefix ":" ln of 
        Nothing -> do
          if active then 
            PS.modify $ BFu.first (<> (T.pack ln <> "\n"))
          else
            interp (T.pack ln)
          loop
        Just ln' -> 
          let (cmd, rest) = BFu.second (dropWhile isSeparator) $ break isSeparator ln' in
          case cmd of 
            "ast" -> outputStrLn (show (parse parseLox "REPL" (T.pack ln'))) *> loop
            "{"   -> do 
              if active then 
                outputStrLn "Can't start multiline while in a multiline"
              else if null rest then 
                PS.put (T.empty, True) 
              else 
                outputStrLn "Beginning of multilines must be empty"
              loop
            "}"   -> do 
              if not (null rest) then 
                outputStrLn "Ends of multiline must be empty"
              else if active then do 
                interp txt -- assume not expression
                PS.put (T.empty, False)
              else 
                outputStrLn "Can't end multiline while not in one"
              loop
            "q" -> pure ()
            "quit" -> pure ()
            bad -> outputStrLn $ "Unknown command " <> bad
interp :: Members [Haskeline, PS.State LxEnv, Error ReturnState, Embed IO] r => T.Text -> Sem r ()
interp inp = do 
  let expr' = parse parseLox "REPL" inp
  env <- PS.get
  case expr' of 
    Left l -> 
      outputStrLn $ errorBundlePretty l
    Right expr -> do
      daNewState <- runFail $ PS.execState env (lxRun expr)
      case daNewState of 
        Left a -> 
          outputStrLn a
        Right newEnv ->
          PS.put newEnv
          
          
