{-|
    Description :  a minimal PolyPaver problem 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    A minimal PolyPaver problem.
-}
module Main where

import PolyPaver

main = 
--    defaultMain problem_exp 
--    defaultMain test_skew 
--    defaultMain sqrt_sin 
    defaultMain sinsin2
    
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
         
sqrt_sin =
    Problem box thm
    where
    box = [(0, (0.000001,1))]
    x = Var 0 "x"
    thm = 
        2 * (sqrt(x+1) - 1) |<=| sin(x)
         
sinsin =
    Problem box thm
    where
    box = [(0, (0.2,1))]
    x = Var 0 "x"
    thm = 
        sin(3*x+1) |<=| sin(sin(3*x)+1) 
         
sinsin2 =
    Problem box thm
    where
    box = [(0, (0.1,0.19))]
    x = Var 0 "x"
    thm = 
        sin(sin(3*x)+1) |<=| sin(3*x+1)

    
    