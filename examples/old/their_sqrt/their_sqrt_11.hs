module Main(main) where

import PolyPaver.Invocation
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = [(0,(409018325532672 % 579004697502343,9015995347763200 % 6369051672525773)),(1,(1 % 2,2 % 1))]
          ,theorem = thm
        }
thm =
    Implies (Geq (Var 0) (Over (Over (Lit (1023 % 1)) (Lit (1024 % 1))) (Sqrt (Var 1)))) (Implies (Leq (Var 0) (Over (Over (Lit (1025 % 1)) (Lit (1024 % 1))) (Sqrt (Var 1)))) (Implies (Geq (Var 1) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Geq (Var 1) (Over (Lit (1 % 1)) (Lit (2 % 1)))) (Implies (Leq (Var 1) (Lit (2 % 1))) (Implies (Geq (Var 0) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 0) (Lit (340282000000000000000000000000000000000 % 1))) (And (Geq (FTimes (Over (Lit (1 % 1)) (Lit (2 % 1))) (Var 0)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Leq (FTimes (Over (Lit (1 % 1)) (Lit (2 % 1))) (Var 0)) (Lit (340282000000000000000000000000000000000 % 1))))))))))

