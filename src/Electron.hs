module Electron where

import System.Environment (getArgs)
import System.IO
import YourApp            (start)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port] <- getArgs
    start (read port)