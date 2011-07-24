{-|
    The executable.  Dispatch to one of several main functions
    depending on the first command-line parameter.
-}
module Main where

import qualified Main.OI

import System.Environment

main =
    do
    args <- getArgs
    case args of
        ("oi" : _) -> Main.OI.main
        _ -> putStrLn "run with one of the following parameters: oi"
