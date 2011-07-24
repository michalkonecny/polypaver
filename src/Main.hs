{-|
    The executable.  Dispatch to one of several main functions
    depending on the first command-line parameter.
-}
module Main where

import qualified Main.Plotter
import qualified Main.Prover

import System.Environment

main =
    do
    args <- getArgs
    case args of
        ("plot" : _) -> Main.Plotter.main
        ("prove" : _) -> Main.Prover.main
        _ -> putStrLn "run with one of the following parameters: oi"
