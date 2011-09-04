module Main(main) where

import PolyPaver.Paver
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = [(0,(0 % 1,1 % 1)),(1,(0 % 1,1 % 1)),(2,(0 % 1,1 % 1)),(3,((-11) % 10,(-1) % 5)),(4,((-11) % 10,11 % 10)),(5,(0 % 1,10 % 1)),(6,(0 % 1,1 % 1)),(7,((-340282000000000000000000000000000000000) % 1,10 % 1))]
          ,theorem = thm
        }
thm =
    Implies (Geq (Var 0 "y1") (Lit (0 % 1))) (Implies (Leq (Var 0 "y1") (Lit (1 % 1))) (Implies (Geq (Var 1 "y2") (Lit (0 % 1))) (Implies (Leq (Var 1 "y2") (Lit (1 % 1))) (Implies (Geq (Var 2 "y3") (Lit (0 % 1))) (Implies (Leq (Var 2 "y3") (Lit (1 % 1))) (Implies (Or (And (Eq (Var 6 "m1__1") (Var 0 "y1")) (Geq (Var 0 "y1") (Var 2 "y3"))) (And (Eq (Var 6 "m1__1") (Var 2 "y3")) (Geq (Var 2 "y3") (Var 0 "y1")))) (Implies (Geq (Plus (Minus (Var 3 "a__2") (Var 4 "b__2")) (Minus (Var 1 "y2") (Var 0 "y1"))) (Neg (Over (Lit (1 % 1)) (Lit (1000000 % 1))))) (Implies (Leq (Plus (Minus (Var 3 "a__2") (Var 4 "b__2")) (Minus (Var 1 "y2") (Var 0 "y1"))) (Over (Lit (1 % 1)) (Lit (1000000 % 1)))) (Implies (Geq (Plus (Plus (Var 3 "a__2") (Var 4 "b__2")) (Minus (Var 1 "y2") (Var 2 "y3"))) (Neg (Over (Lit (1 % 1)) (Lit (1000000 % 1))))) (Implies (Leq (Plus (Plus (Var 3 "a__2") (Var 4 "b__2")) (Minus (Var 1 "y2") (Var 2 "y3"))) (Over (Lit (1 % 1)) (Lit (1000000 % 1)))) (Implies (Geq (Var 1 "y2") (Neg (Over (Lit (11 % 1)) (Lit (10 % 1))))) (Implies (Leq (Var 1 "y2") (Over (Lit (11 % 1)) (Lit (10 % 1)))) (Implies (Geq (Var 1 "y2") (Neg (Over (Lit (6 % 1)) (Lit (5 % 1))))) (Implies (Leq (Var 1 "y2") (Over (Lit (6 % 1)) (Lit (5 % 1)))) (Implies (Geq (Var 7 "m2__3") (Minus (Var 1 "y2") (Over (Lit (7 % 1)) (Lit (10 % 1))))) (Implies (Or (And (Eq (Var 5 "r__4") (Var 6 "m1__1")) (Geq (Var 6 "m1__1") (Var 7 "m2__3"))) (And (Eq (Var 5 "r__4") (Var 7 "m2__3")) (Geq (Var 7 "m2__3") (Var 6 "m1__1")))) (Implies (Geq (Var 5 "r__4") (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 5 "r__4") (Lit (340282000000000000000000000000000000000 % 1))) (And (And (Geq (Var 5 "r__4") (Minus (Var 0 "y1") (Lit (1 % 1)))) (Geq (Var 5 "r__4") (Minus (Var 1 "y2") (Lit (1 % 1))))) (Geq (Var 5 "r__4") (Minus (Var 2 "y3") (Lit (1 % 1)))))))))))))))))))))))

