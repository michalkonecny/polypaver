{-|
    The executable.  Dispatch to one of several main functions
    depending on the first command-line parameter.
-}
module Main where

import qualified Main.Prover
import qualified Main.Plotter

import System.Environment

main =
    do
    args <- getArgs
    case args of
        ("prove" : _) -> Main.Prover.main
        ("plot" : _) -> Main.Plotter.main
        _ -> putStrLn "run with one of the following parameters: prove plot"
