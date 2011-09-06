module Main(main) where

import PolyPaver.Paver
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = []
          ,conjecture = thm
        }
thm =
    Eq (Lit (1 % 1)) (Lit (0 % 1))
