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
    defaultMain Problem 
        {box = test_box
        ,ivars = []
        ,theorem = test_thm}

test_box = [(0, (-1,1))]

x = Var 0

test_thm = x |<=| cos(x)+1
