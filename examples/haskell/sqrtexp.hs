{-|
    Description :  scalable properties involving elementary functions 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    Scalable properties involving elementary functions.
-}

{- 
    There are several ways to invoke PolyPaver on problem defined below:
    * compile this module and execute it
    * run the polypaver executable and pass it this file as parameter, eg:
        polypaver examples/haskell/integral.hs problemSqrt
        
    To prove problemSqrt and problemExp, use eg the following parameters: 
    -d 2 -m 4 -w 1000 -h 1000
-}

module Main where

import PolyPaver

main = defaultMain problem

problem = problemSqrt

problemExp = 
    Problem 
       {box = bench_exp_box
       ,conjecture = bench_exp}

problemSqrt = 
    Problem 
        {box = bench_sqrt_box
        ,conjecture = bench_sqrt}
        
indices = [0..1]

distance = 0.5^^14

bench_sqrt_box = [(i, (1,2), False) | i <- indices]

bench_sqrt =
      (sqrt . product . map var $ indices) -- sqrt(x_1 * ... * x_n)
      |<|
      (product . map (sqrt . var) $ indices) -- sqrt(x_1) * ... * sqrt(x_n)
      + (fromRational distance)

bench_exp_box = [(i, (0,1), False) | i <- indices]

bench_exp =
      (product . map (exp . var) $ indices) -- exp(x_1) * ... * exp(x_n)
      |<|
      (exp . sum $ fromRational distance : map var indices) -- exp(x_1 + ... + x_n)
      
var i = termVar i ("x" ++ show i)

