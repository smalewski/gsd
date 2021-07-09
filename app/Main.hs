{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)

import Server (app)
import Data.Text.IO (readFile)
import qualified Network.Wai.Handler.Warp as W
import Cmd
import Interpreter as I
import Interpreter.Syntax.EvCore (Expr)
import Interpreter.Printer (ppr)
import Interpreter.Printer.Latex

main :: IO ()
main = execCmd =<< runCmd --run 8001 app

execCmd :: Cmd -> IO ()
execCmd (Server port) = W.run port app
execCmd (Eval s f) = do
  src <- readFile f
  res <- run s False src
  printResult res

printResult :: Either OutError (Res Expr) -> IO ()
printResult (Left e) = print $ ppr e
printResult (Right (Res e t _)) = print $ ppr e <+> ":" <+> ppr t
