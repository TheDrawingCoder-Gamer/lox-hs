{-# LANGUAGE DeriveDataTypeable #-}
module Lox.ArgParser where

import System.Console.CmdArgs.Implicit

data Options 
   = OptREPL
   | OptCompile {
      optFile :: String 
      ,optSaveTo :: String} 
  deriving Data

goodArgs = cmdArgs $ modes [  OptREPL &= name "repl", 
  OptCompile 
    { optFile = def &= argPos 0 &= typFile
    , optSaveTo = def &= argPos 1 &= typFile} &= name "compile" ] 
