{-|
    Description :  main sqrt CV from 2011 AMAI paper  
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    sqrt43 benchmark
    
    For precision -r 10 proved with -d 3 -e 10.
-}

module Main where

import PolyPaver

main = defaultMain problem 

problem =
    Problem 
        {box = b
        ,conjecture = test}

b = [(0, (0.5,2), False), (1, (0,3), False)]

x = termVar 0 "x"

r = termVar 1 "r"

test =
    r |<-| hull (-x^2/4+x) (x^2/4+1) /\ 
    r |==| (0.5 *: (r +: x /: r))
    ---> 
    0.5 *: (r +: x /: r) |<-| (1+4*fepsiRel) * sqrt(x)
