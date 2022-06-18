module Main where

import Lox.Parser 
import Lox.Interp
import Lox.Types
import Lox.Helpers
import Text.Megaparsec
import Data.Either (fromRight)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Polysemy.Haskeline
import Polysemy hiding (interpret)
import Polysemy.Embed
import Polysemy.State qualified as PS
import Polysemy.Reader qualified as PR
import Polysemy.Fail
import Polysemy.Error
import Polysemy.Counter
import Polysemy.Trace
import Data.Map.Strict qualified as M
import Data.List (stripPrefix)
import Control.Monad (void)
import Lox.NativeFun
import Data.Char (isSeparator)
import Data.Bifunctor qualified as BFu
import Data.Bitraversable qualified as BFt
import Polysemy.Fixpoint
import Lox.ArgParser
import Lox.Compiler
import Data.Functor (($>))
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.ByteString.Lazy qualified as B
main :: IO ()
main = do 
  opts <- goodArgs
  case opts of 
    OptREPL -> void $ runFinal $ fixpointToFinal . runError . runCounter . PS.evalState (T.empty, False) . PS.evalState M.empty . PS.evalState M.empty . PS.evalState M.empty . PS.evalState M.empty $ haskelineToIOFinal loop
    OptCompile file saveTo -> do 
      fileC <- T.readFile file
      bytecode <- runM . traceToStderr $ compileFile file fileC
      case bytecode of 
        Nothing -> putStrLn "failed compile"
        Just v -> B.writeFile saveTo v
compileFile :: Member Trace r => String -> T.Text -> Sem r (Maybe B.ByteString)
compileFile filename file = do
  let stmts = parse parseLoxFile filename file 
  case stmts of 
    Left e -> do
      trace (errorBundlePretty e)
      pure Nothing
    Right ss -> 
      let compiled = compileBytes ss in
      case compiled of 
        Left e -> do
          trace . showCompileError $ e 
          pure Nothing
        Right e -> pure (Just e)
loop :: Sem [Haskeline, PS.State Heap, PS.State Stack, PS.State RefHeap, PS.State FunClosures, PS.State (T.Text, Bool), Counter, Error InterpError, Fixpoint, Final IO] () 
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
            PS.modify @(T.Text, Bool) $ BFu.first (<> (T.pack ln <> "\n"))
          else
            interp (T.pack ln)
          loop
        Just ln' -> 
          let (cmd, rest) = BFu.second (dropWhile isSeparator) $ break isSeparator ln' in
          case cmd of 
            "ast" -> do 
              let ast = parse parseLox "REPL" (T.pack rest) 
              case ast of 
                Left e -> 
                  outputStrLn (errorBundlePretty e) 
                Right e -> 
                  outputStrLn (show e) 
              loop
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
            "heap" -> (outputStrLn . show . M.map prettyLoxValue =<< PS.get @Heap) *> loop
            "refheap" -> (outputStrLn . show . M.map prettyLoxRefValue =<< PS.get @RefHeap) *> loop
            "stack" -> (outputStrLn . show =<< PS.get @Stack) *> loop
            "vars"  -> (outputStrLn . show . M.map prettyLoxValue =<< (M.compose <$> PS.get @Heap <*> PS.get @Stack)) *> loop
            "closures" -> (outputStrLn . show =<< PS.get @FunClosures) *> loop
            "r" -> PS.put @Heap M.empty *> PS.put @Stack M.empty *> PS.put @FunClosures M.empty *> PS.put @RefHeap M.empty *> loop
            bad -> outputStrLn ("Unknown command " <> bad) *> loop
interp :: Members [Haskeline, PS.State Heap, PS.State Stack, PS.State RefHeap, PS.State FunClosures, Counter, Error InterpError, Final IO, Fixpoint] r => T.Text -> Sem r ()
interp inp = do 
  let expr' = parse parseLox "REPL" inp
  stack <- PS.get @Stack
  heap  <- PS.get @Heap
  case expr' of 
    Left l -> 
      outputStrLn $ errorBundlePretty l
    Right expr -> do
      val <- traceToHaskeline $ runError @InterpError (PR.runReader stack (interpret expr))
      case val of 
        Left e -> 
          outputStrLn $ showInterpError e 
        Right newStack -> PS.put @Stack newStack


guardEmpty :: Member Haskeline r => String -> Sem r a -> Sem r (Maybe a)
guardEmpty [] _ = outputStrLn "Rest of line must be empty" $> Nothing
guardEmpty _ sem = Just <$> sem
