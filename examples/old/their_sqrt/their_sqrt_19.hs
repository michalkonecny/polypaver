module Main(main) where

import PolyPaver.Invocation
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = [(0,(409417757491200 % 579004697502343,529835755569152 % 374650098383869)),(1,(1 % 2,2 % 1))]
          ,theorem = thm
        }
thm =
    Implies (Geq (Var 0) (Over (Over (Lit (1048575 % 1)) (Lit (1048576 % 1))) (Sqrt (Var 1)))) (Implies (Leq (Var 0) (Over (Over (Lit (1048577 % 1)) (Lit (1048576 % 1))) (Sqrt (Var 1)))) (Implies (Geq (Var 1) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Geq (Var 1) (Over (Lit (1 % 1)) (Lit (2 % 1)))) (Implies (Leq (Var 1) (Lit (2 % 1))) (Implies (Geq (Var 0) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 0) (Lit (340282000000000000000000000000000000000 % 1))) (And (Geq (FTimes (Over (Lit (1 % 1)) (Lit (2 % 1))) (Var 0)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Leq (FTimes (Over (Lit (1 % 1)) (Lit (2 % 1))) (Var 0)) (Lit (340282000000000000000000000000000000000 % 1))))))))))

