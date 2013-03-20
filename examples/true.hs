module Main(main) where

import PolyPaver

main =
    defaultMain Problem
        {
          box = []
          ,conjecture = thm
        }
thm =
    0 |=| 0
