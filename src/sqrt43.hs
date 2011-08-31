{-|
    Module      :  Main
    Description :  scalable properties involving elementary functions 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    sqrt43 benchmark
-}

module Main 
(
  main
)
where

import PolyPaver.Paver

main = 
    defaultMain Problem 
        {box = b
        ,theorem = test}

b = [(0, (0.5,2)), (1, (0,3))]

x = Var 0

r = Var 1

test =
    r |<-| Hull (-x^2/4+x) (x^2/4+1) /\ 
    Eq r (0.5 *: (r +: x /: r))
    ---> 
    0.5 *: (r +: x /: r) |<-| (1+4*EpsiRel) * sqrt(x)
