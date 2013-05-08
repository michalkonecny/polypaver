module Main where

import PolyPaver

main =
    defaultMain problem
    
problem =    
        Problem
        {
          box = []
          ,conjecture = form
        }

form = 0 |=| 1
