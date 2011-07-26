
module Main where

import Paver

main = 
    defaultMain Problem 
        {box = 
            [
                (0, (-3,3)),
                (1, (-3,3))
            ]
        ,ivars = []
        ,theorem = teddy}
    
x = Var 0
y = Var 1

teddy = Not $ 
  Or lefteye $
  Or righteye $
  Or nose $
  Or smile $
  Not $
  Or face $
  Or leftear rightear

face = 
  circle 0 0 2

leftear =
  (circle (-1.5) 1.5 0.8) `And` ((y - x) `Geq` 2.5)

rightear =
  (circle 1.5 1.5 0.8) `And` ((x + y) `Geq` 2.5)

lefteye =
  (circle (-1) 1 0.4) `And` (Not $ circle (-0.9) 0.9 0.05)

righteye =
  (circle 1 1 0.4) `And` (Not $ circle 0.9 0.9 0.05)

nose =
  (circle 0 0.3 0.4) `And` (y `Geq` 0.3)

smile =
  (circle 0 0 1) `And` (y `Leq` 0)

-- centre (a,b) radius r 
circle a b r =
  ((x-a)^2 + (y-b)^2) `Leq` (r^2)
