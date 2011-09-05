module Main(main) where

import PolyPaver.Paver
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = [(0,((-32769) % 1048576,32769 % 1048576))]
          ,theorem = thm
        }
thm =
    Implies (Geq (Var 0) (Neg (Over (Lit (32769 % 1)) (Lit (1048576 % 1))))) (Implies (Leq (Var 0) (Over (Lit (32769 % 1)) (Lit (1048576 % 1)))) (Implies (Geq (Var 0) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 0) (Lit (340282000000000000000000000000000000000 % 1))) (And (Geq (FTimes (Var 0) (Var 0)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Leq (FTimes (Var 0) (Var 0)) (Lit (340282000000000000000000000000000000000 % 1)))))))

