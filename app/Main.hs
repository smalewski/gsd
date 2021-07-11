{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile, putStrLn)

import Server (app)
import qualified Network.Wai.Handler.Warp as W
import Cmd
import Interpreter as I
import Interpreter.Type (Type)
import Interpreter.Syntax.EvCore (Expr)
import Interpreter.Printer.Plain (Printable(ppr))
import Data.Text (Text)
import Data.Text.IO (readFile, putStrLn)

main :: IO ()
main = execCmd =<< runCmd --run 8001 app

execCmd :: Cmd -> IO ()
execCmd (Server port) = W.run port app
execCmd (Eval valid format filename) = do
  src <- readFile filename
  res <- run valid False src
  printResult res

printResult :: Either OutError (Res Expr) -> IO ()
printResult (Left e) = putStrLn $ ppr e
printResult (Right (Res e t _)) = putStrLn $ ppr e <> " : " <> ppr t
