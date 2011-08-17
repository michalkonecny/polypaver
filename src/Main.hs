
module Main 
(
  main
)
where

import Paver

main = 
--    defaultMain Problem 
--        {box = bench_exp_box
--        ,ivars = []
--        ,theorem = bench_exp}
    defaultMain Problem 
        {box = bench_sqrt_box
        ,ivars = []
        ,theorem = bench_sqrt}

indices = [0..1]

distance = 0.5^^10

bench_sqrt_box = [(i, (1,2)) | i <- indices]

bench_sqrt =
      (Sqrt . product . map Var $ indices)
      `Le`
      (Plus (Lit distance) $ product . map (Sqrt . Var) $ indices)

bench_exp_box = [(i, (0,1)) | i <- indices]

bench_exp =
      (product . map (Exp . Var) $ indices)
      `Le`
      (Exp . sum $ Lit distance : map Var indices)

