module Main(main) where

import PolyPaver.Paver
import Data.Ratio ((%))

main =
    defaultMain Problem
        {
          box = [(0,(24083420467200 % 34059099853079,9007199254749184 % 6369051672525773)),(1,(1 % 2,2 % 1))]
          ,theorem = thm
        }
thm =
    Implies (Geq (Var 0) (Over (Over (Lit (1099511627775 % 1)) (Lit (1099511627776 % 1))) (Sqrt (Var 1)))) (Implies (Leq (Var 0) (Over (Over (Lit (1099511627777 % 1)) (Lit (1099511627776 % 1))) (Sqrt (Var 1)))) (Implies (Geq (Var 1) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Geq (Var 1) (Over (Lit (1 % 1)) (Lit (2 % 1)))) (Implies (Leq (Var 1) (Lit (2 % 1))) (Implies (Geq (Var 0) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (Var 0) (Lit (340282000000000000000000000000000000000 % 1))) (Implies (Geq (FTimes (Var 1) (Var 0)) (Neg (Lit (340282000000000000000000000000000000000 % 1)))) (Implies (Leq (FTimes (Var 1) (Var 0)) (Lit (340282000000000000000000000000000000000 % 1))) (And (Geq (FTimes (Var 1) (Var 0)) (Times (Over (Lit (8796093022207 % 1)) (Lit (8796093022208 % 1))) (Sqrt (Var 1)))) (Leq (FTimes (Var 1) (Var 0)) (Times (Over (Lit (8796093022209 % 1)) (Lit (8796093022208 % 1))) (Sqrt (Var 1)))))))))))))

