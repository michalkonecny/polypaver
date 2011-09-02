module Main(main) where

import PolyPaver.Paver
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = [(0,(0 % 1,1 % 1)),(1,(0 % 1,1 % 1)),(2,(0 % 1,1 % 1)),(3,((-11) % 10,11 % 10)),(4,((-11) % 10,11 % 10)),(5,(0 % 1,1 % 1))]
          ,theorem = thm
        }
thm =
    Implies (Geq (Var 0) (Lit (0 % 1))) (Implies (Leq (Var 0) (Lit (1 % 1))) (Implies (Geq (Var 1) (Lit (0 % 1))) (Implies (Leq (Var 1) (Lit (1 % 1))) (Implies (Geq (Var 2) (Lit (0 % 1))) (Implies (Leq (Var 2) (Lit (1 % 1))) (Implies (Or (And (Eq (Var 5) (Var 0)) (Geq (Var 0) (Var 2))) (And (Eq (Var 5) (Var 2)) (Geq (Var 2) (Var 0)))) (Implies (Geq (Var 5) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 5) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (Plus (Minus (Var 3) (Var 4)) (Minus (Var 1) (Var 0))) (Neg (Over (Lit (1 % 1)) (Lit (1000000 % 1))))) (Implies (Leq (Plus (Minus (Var 3) (Var 4)) (Minus (Var 1) (Var 0))) (Over (Lit (1 % 1)) (Lit (1000000 % 1)))) (Implies (Geq (Plus (Plus (Var 3) (Var 4)) (Minus (Var 1) (Var 2))) (Neg (Over (Lit (1 % 1)) (Lit (1000000 % 1))))) (Implies (Leq (Plus (Plus (Var 3) (Var 4)) (Minus (Var 1) (Var 2))) (Over (Lit (1 % 1)) (Lit (1000000 % 1)))) (Implies (Geq (Var 3) (Neg (Over (Lit (11 % 1)) (Lit (10 % 1))))) (Implies (Leq (Var 3) (Over (Lit (11 % 1)) (Lit (10 % 1)))) (Implies (Geq (Var 4) (Neg (Over (Lit (11 % 1)) (Lit (10 % 1))))) (Implies (Leq (Var 4) (Over (Lit (11 % 1)) (Lit (10 % 1)))) (Implies (Geq (Var 1) (Neg (Over (Lit (11 % 1)) (Lit (10 % 1))))) (Implies (Leq (Var 1) (Over (Lit (11 % 1)) (Lit (10 % 1)))) (Implies (Geq (Var 4) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 4) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (Neg (Times (Lit (2 % 1)) (Var 3))) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Neg (Times (Lit (2 % 1)) (Var 3))) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Or (Or (Leq (Neg (Over (Lit (1 % 1)) (Lit (5 % 1)))) (Var 3)) (Leq (Var 4) (Times (Lit (2 % 1)) (Var 3)))) (Leq (Neg (Times (Lit (2 % 1)) (Var 3))) (Var 4))) (And (And (Geq (Var 5) (Minus (Var 0) (Lit (1 % 1)))) (Geq (Var 5) (Minus (Var 1) (Lit (1 % 1))))) (Geq (Var 5) (Minus (Var 2) (Lit (1 % 1))))))))))))))))))))))))))))

