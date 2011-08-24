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
    x = Var 0
    thm = exp(x) |<=| exp(x+0.0001) 

test_skew =
    Problem box thm 
    where
    box = [(0, (1,4)), (1, (1,4))]
    x = Var 0
    y = Var 1
    thm = 
        x |<=| y*y
        --->
        x |<=| y*y + 0.01
--        exp x |<=| exp (y*y) + 0.1
         

    
    