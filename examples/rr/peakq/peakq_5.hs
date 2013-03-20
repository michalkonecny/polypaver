module Main(main) where

import PolyPaver.Invocation
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = [(0,((-6) % 5,(-1) % 10)),(1,((-6) % 5,6 % 5)),(2,((-6) % 5,6 % 5)),(3,((-1) % 1,1 % 1))]
          ,theorem = thm
        }
thm =
    Implies (Geq (Var 3) (Neg (Lit (1 % 1)))) (Implies (Leq (Var 3) (Lit (1 % 1))) (Implies (Le (Var 0) (Neg (Over (Lit (1 % 1)) (Lit (10 % 1))))) (Implies (Geq (Var 0) (Neg (Over (Lit (6 % 1)) (Lit (5 % 1))))) (Implies (Leq (Var 0) (Over (Lit (6 % 1)) (Lit (5 % 1)))) (Implies (Geq (Var 1) (Neg (Over (Lit (6 % 1)) (Lit (5 % 1))))) (Implies (Leq (Var 1) (Over (Lit (6 % 1)) (Lit (5 % 1)))) (Implies (Geq (Var 2) (Neg (Over (Lit (6 % 1)) (Lit (5 % 1))))) (Implies (Leq (Var 2) (Over (Lit (6 % 1)) (Lit (5 % 1)))) (Implies (Geq (Var 0) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 0) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (Var 1) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 1) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (Var 2) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 2) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (FTimes (Var 1) (Var 1)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FTimes (Var 1) (Var 1)) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (FTimes (Lit (4 % 1)) (Var 0)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FTimes (Lit (4 % 1)) (Var 0)) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (FTimes (FTimes (Var 1) (Var 1)) (FTimes (Lit (4 % 1)) (Var 0))) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FTimes (FTimes (Var 1) (Var 1)) (FTimes (Lit (4 % 1)) (Var 0))) (Lit (340282000000000000000000000000000000000 % 1))) (And (Geq (FMinus (Var 2) (FTimes (FTimes (Var 1) (Var 1)) (FTimes (Lit (4 % 1)) (Var 0)))) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Leq (FMinus (Var 2) (FTimes (FTimes (Var 1) (Var 1)) (FTimes (Lit (4 % 1)) (Var 0)))) (Lit (340282000000000000000000000000000000000 % 1))))))))))))))))))))))))

