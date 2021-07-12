{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MonoLocalBinds #-}
module Main where

import Prelude hiding (readFile, putStrLn)

import Server (app)
import qualified Network.Wai.Handler.Warp as W
import Cmd
import Interpreter as I
import Interpreter.Type (Type)
import Interpreter.Syntax.EvCore (Expr)
import Interpreter.Printer (Format, Printable(..))
import Data.Text (Text)
import Data.Text.IO (readFile, putStrLn)

main :: IO ()
main = execCmd =<< runCmd --run 8001 app

execCmd :: Cmd -> IO ()
execCmd (Server port) = W.run port app
execCmd (Eval Evaluate valid format filename) = do
  src <- readFile filename
  res <- run valid False src
  printResult format res
execCmd (Eval Typecheck valid format filename) = do
  src <- readFile filename
  res <- check valid src
  printResult format res

printResult :: Printable a => Format -> Either OutError (Res a) -> IO ()
printResult fmt (Left e) = putStrLn $ ppr fmt e
printResult fmt (Right (Res e t _)) = putStrLn $ ppr fmt e <> " : " <> ppr fmt t
