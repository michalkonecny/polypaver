{-|
    Module      :  Main
    Description :  scalable properties involving elementary functions 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    Scalable properties involving elementary functions.
-}

module Main 
(
  main
)
where

import PolyPaver.Paver

main = 
--    defaultMain Problem 
--        {box = bench_exp_box
--        ,ivars = []
--        ,theorem = bench_exp}
    defaultMain Problem 
        {box = bench_sqrt_box
        ,theorem = bench_sqrt}

indices = [0..1]

distance = 0.5^^14

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

