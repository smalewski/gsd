{-# LANGUAGE DeriveDataTypeable #-}
module Cmd where

import System.Console.CmdArgs
import Interpreter.Syntax.Common
import Interpreter.Printer (Format(..))

data Cmd
  = Server { port :: Int }
  | Eval { strategy :: Valid, format :: Format, file :: FilePath }
  deriving (Data, Typeable, Show, Eq)

evalCmd = Eval { strategy = enum [ Complete &= help "Complete strategy"
                                 , Exact &= help "Exact strategy"
                                 , Sound &= help "Sound strategy"
                                 ] &= groupname "Matching strategies"
               , format = enum [ Plain &= help "Plain text"
                               , Latex &= help "Latex formated"
                               ] &= groupname "Output formats"
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
