{-|
    Description :  scalable properties involving elementary functions 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    Scalable properties involving elementary functions.
-}

module Main where

import PolyPaver

main = 
--    defaultMain Problem 
--        {box = bench_exp_box
--        ,conjecture = bench_exp}
    defaultMain Problem 
        {box = bench_sqrt_box
        ,conjecture = bench_sqrt}

indices = [0..1]

distance = 0.5^^14

bench_sqrt_box = [(i, (1,2), False) | i <- indices]

bench_sqrt =
      (sqrt . product . map var $ indices)
      |<|
      (fromRational distance) + (product . map (sqrt . var) $ indices)

bench_exp_box = [(i, (0,1), False) | i <- indices]

bench_exp =
      (product . map (exp . var) $ indices)
      |<|
      (exp . sum $ fromRational distance : map var indices)
      
var i = termVar i ("x" ++ show i)

