module Main(main) where

import PolyPaver

main =
    defaultMain Problem
        {
          box = []
          ,conjecture = conjecture
        }
conjecture =
    0 |=| 0
