{-|
    Module      :  Main
    Description :  a minimal PolyPaver problem 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    A minimal PolyPaver problem.
-}
module Main 
(
  main
)
where

import PolyPaver.Paver

main = 
--    defaultMain problem_exp 
    defaultMain test_skew 
    
exp_shift =
    Problem box thm 
    where
    box = [(0, (-1,1))]
    x = Var 0 "x"
    thm = exp(x) |<=| exp(x+0.0001) 

test_skew =
    Problem box thm 
    where
    box = [(0, (0,4)), (1, (0,4))]
    x = Var 0 "x"
    y = Var 1 "y"
    thm = 
        x |<=| y*y
        --->
        x |<=| y*y + 0.025
--        exp x |<=| exp (y*y) + 0.1
         

    
    