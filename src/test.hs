module Main 
(
  main
)
where

import Paver

main = 
    defaultMain Problem 
        {box = test_box
        ,ivars = []
        ,theorem = test_thm}

test_box = [(0, (0,1))]

x = Var 0

test_thm = x |<=| cos(x)
