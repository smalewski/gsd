module Main where

import Server (app)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Cmd

main :: IO ()
main = print =<< runCmd --run 8001 app
