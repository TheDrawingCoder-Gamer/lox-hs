module Main where

import Lox.Parser 
import Lox.Evaluate
import Lox.Types
import Lox.Helpers
import Lox.Resolver qualified as R
import Text.Megaparsec
import Data.Either (fromRight)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Polysemy.Haskeline
import Polysemy
import Polysemy.Embed
import Polysemy.State qualified as PS
import Polysemy.StackState qualified as PSS
import Polysemy.Fail
import Polysemy.Error
import Data.HashMap.Strict qualified as HM
import Data.List (stripPrefix)
import Control.Monad (void)
import Lox.NativeFun
import Data.Char (isSeparator)
import Data.Bifunctor qualified as BFu
import Data.Bitraversable qualified as BFt
import Polysemy.Fixpoint
main :: IO ()
main = void $ runFinal $ fixpointToFinal . runError . PS.evalState [] . PS.evalState (T.empty, False) . PS.evalState [envWithNative] . PSS.stackStateToState $ haskelineToIOFinal loop
loop :: Sem [Haskeline, PSS.StackState LxEnv, PS.State (T.Text, Bool), PS.State [R.Scope], Error ReturnState, Fixpoint, Final IO] () 
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
            "ast" -> outputStrLn (show (parse parseLox "REPL" (T.pack rest))) *> loop
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
interp :: Members [Haskeline, PSS.StackState LxEnv, PS.State [R.Scope], Error ReturnState, Final IO, Fixpoint] r => T.Text -> Sem r ()
interp inp = do 
  let expr' = parse parseLox "REPL" inp
  env <- PSS.get
  case expr' of 
    Left l -> 
      outputStrLn $ errorBundlePretty l
    Right expr -> do
      newAst <- BFt.bitraverse (runFail . PS.evalState [] . R.resolve) (runFail . PS.evalState [] . R.resolveExpr) expr 
      case newAst of 
        Left s -> 
          case s of 
            Left e -> outputStrLn e
            Right ss -> do
              res <- traceToHaskeline $ runFail (lxStmts ss)
              case res of 
                Left e -> outputStrLn e
                Right e -> pure ()
        Right s -> 
          case s of 
            Left e -> outputStrLn e
            Right e -> do 
              res <- traceToHaskeline $ runFail (lxEvaluate e) 
              case res of 
                Left e -> outputStrLn e
                Right e -> outputStrLn (prettyLoxValue e)
