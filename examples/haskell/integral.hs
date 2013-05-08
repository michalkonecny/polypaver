{-|
    Description :  a simple property involving an integral 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    A simple property involving an integral.
-}

{- 
    There are several ways to invoke PolyPaver on problem defined below:
    * compile this module and execute it
    * run the polypaver executable and pass it this file as parameter, eg:
        polypaver examples/haskell/integral.hs
-}

module Main where

import PolyPaver

main = defaultMain problem

problem = Problem 
        {box = b
        ,conjecture = test}

b = [(xNum, (0,1), False), (yNum, (0,1), False)]
x = termVar xNum "x"
y = termVar yNum "y"
t = termVar tNum "t"
(xNum : yNum : tNum : _) = [0..]

test =
    x + -- /80 + -- uncomment to make it false
    (y * (y + 2 * x) ) / 2
    |<-|
--    (plusMinus 0.1) + -- uncomment to make it non-touching
    integral tNum "t" x (x + y) t
     