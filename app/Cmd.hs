{-# LANGUAGE DeriveDataTypeable #-}
module Cmd where

import System.Console.CmdArgs
import Interpreter.Syntax.Common

data Cmd
  = Server { port :: Int }
  | Eval { strategy :: Valid, file :: FilePath }
  deriving (Data, Typeable, Show, Eq)

evalCmd = Eval { strategy = enum [ Complete &= help "Complete strategy"
                                 , Exact &= help "Exact strategy"
                                 , Sound &= help "Sound strategy"
                                 ] &= groupname "Matching strategies"
               , file = def &= typFile &= argPos 0
               }

serverCmd = Server { port = 8001 &= typ "NUM" &= help "Default port: 8001" }

mode = cmdArgsMode $ modes [ evalCmd &= auto &= help "Evaluate source file."
                           , serverCmd &= help "Launch interpreter in a web server"
                                       &= help ""]
     &= help ""
     &= program "gsd"
     &= summary "Interpreter for the GSD language"

runCmd = cmdArgsRun mode
