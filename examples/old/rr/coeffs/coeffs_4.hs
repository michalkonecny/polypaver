module Main(main) where

import PolyPaver.Invocation
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = [(0,(0 % 1,1 % 1)),(1,(0 % 1,1 % 1)),(2,(0 % 1,1 % 1))]
          ,theorem = thm
        }
thm =
    Implies (Geq (Var 0) (Lit (0 % 1))) (Implies (Leq (Var 0) (Lit (1 % 1))) (Implies (Geq (Var 1) (Lit (0 % 1))) (Implies (Leq (Var 1) (Lit (1 % 1))) (Implies (Geq (Var 2) (Lit (0 % 1))) (Implies (Leq (Var 2) (Lit (1 % 1))) (Implies (Geq (FTimes (Neg (Lit (2 % 1))) (Var 1)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FTimes (Neg (Lit (2 % 1))) (Var 1)) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (FPlus (FTimes (Neg (Lit (2 % 1))) (Var 1)) (Var 2)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FPlus (FTimes (Neg (Lit (2 % 1))) (Var 1)) (Var 2)) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (FPlus (Var 0) (FPlus (FTimes (Neg (Lit (2 % 1))) (Var 1)) (Var 2))) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FPlus (Var 0) (FPlus (FTimes (Neg (Lit (2 % 1))) (Var 1)) (Var 2))) (Lit (340282000000000000000000000000000000000 % 1))) (And (Geq (FTimes (Over (Lit (1 % 1)) (Lit (2 % 1))) (FPlus (Var 0) (FPlus (FTimes (Neg (Lit (2 % 1))) (Var 1)) (Var 2)))) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Leq (FTimes (Over (Lit (1 % 1)) (Lit (2 % 1))) (FPlus (Var 0) (FPlus (FTimes (Neg (Lit (2 % 1))) (Var 1)) (Var 2)))) (Lit (340282000000000000000000000000000000000 % 1)))))))))))))))

