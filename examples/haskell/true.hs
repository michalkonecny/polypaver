module Main where

import PolyPaver

main =
    defaultMain problem

problem =
    Problem
        {
          box = []
          ,conjecture = conjecture
        }
    where
    conjecture =
        0 |=| 0
