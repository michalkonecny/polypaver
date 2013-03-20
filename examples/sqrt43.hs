{-|
    Description :  main sqrt CV from 2011 AMAI paper  
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    sqrt43 benchmark
-}

module Main where

import PolyPaver

main = 
    defaultMain Problem 
        {box = b
        ,conjecture = test}

b = [(0, (0.5,2), False), (1, (0,3), False)]

x = Var 0 "x"

r = Var 1 "r"

test =
    r |<-| Hull (-x^2/4+x) (x^2/4+1) /\ 
    Eq r (0.5 *: (r +: x /: r))
    ---> 
    0.5 *: (r +: x /: r) |<-| (1+4*EpsiRel) * sqrt(x)
