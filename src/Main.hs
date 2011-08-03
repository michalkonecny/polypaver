
module Main 
(
  main
)
where

import Paver

main = 
    defaultMain Problem 
        {box = [(i, (0,1)) | i <- indices]
        ,ivars = []
        ,theorem = bench}

indices = [0..1]

distance = 0.5^^6

bench =
      (product . map (Exp . Var) $ indices)
      `Le`
      (Exp . sum $ Lit distance : map Var indices)


