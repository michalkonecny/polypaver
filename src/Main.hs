
module Main where

import Paver

main = 
    defaultMain Problem 
        {box = [(i, (0,1)) | i <- indices]
        ,ivars = []
        ,theorem = bench}

indices = [0..1]

r = 0.01

bench =
    Leq
      (product . map (Exp . Var) $ indices)
      (Exp . sum $ Lit r : map Var indices)


