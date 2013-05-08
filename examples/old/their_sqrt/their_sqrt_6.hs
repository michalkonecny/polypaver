module Main(main) where

import PolyPaver.Invocation
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = [(0,(1 % 2,2 % 1)),(1,(633318697598976 % 909864524646539,9147936743096320 % 6369051672525773))]
          ,theorem = thm
        }
thm =
    Implies (Geq (Var 0) (Over (Lit (1 % 1)) (Lit (2 % 1)))) (Implies (Leq (Var 0) (Lit (2 % 1))) (Implies (Geq (Var 0) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Geq (Var 1) (Over (Over (Lit (63 % 1)) (Lit (64 % 1))) (Sqrt (Var 0)))) (Implies (Leq (Var 1) (Over (Over (Lit (65 % 1)) (Lit (64 % 1))) (Sqrt (Var 0)))) (Implies (Geq (Var 1) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 1) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (FTimes (Over (Lit (1 % 1)) (Lit (2 % 1))) (Var 1)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FTimes (Over (Lit (1 % 1)) (Lit (2 % 1))) (Var 1)) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (FTimes (Var 1) (Var 1)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FTimes (Var 1) (Var 1)) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (FTimes (FTimes (Var 1) (Var 1)) (Var 0)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FTimes (FTimes (Var 1) (Var 1)) (Var 0)) (Lit (340282000000000000000000000000000000000 % 1))) (And (Geq (FMinus (Lit (3 % 1)) (FTimes (FTimes (Var 1) (Var 1)) (Var 0))) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Leq (FMinus (Lit (3 % 1)) (FTimes (FTimes (Var 1) (Var 1)) (Var 0))) (Lit (340282000000000000000000000000000000000 % 1))))))))))))))))

