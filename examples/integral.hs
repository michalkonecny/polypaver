{-|
    Description :  a simple property involving an integral 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    a simple property involving an integral
-}

module Main where

import PolyPaver

main = 
    defaultMain Problem 
        {box = b
        ,conjecture = test}

b = [(xNum, (0,1), False), (yNum, (0,1), False)]
x = Var xNum "x"
y = Var yNum "y"
t = Var tNum "t"
(xNum : yNum : tNum : _) = [0..]

test =
--    x/80 + 
    (y * (y + 2 * x) ) / 2
    |<-|
    (plusMinus 0.01) + 
    Integral x (x + y) tNum "t" t 