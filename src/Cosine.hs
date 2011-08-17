
module Main 
(
  main
)
where

import Paver

main = 
    defaultMain Problem 
        {box = cosine_box
        ,ivars = []
        ,theorem = cosine_thm}

cosine_box = 
  [
    (0, (-340282000000000000000000000000000000000,
          340282000000000000000000000000000000000))
  ]

x = Var 0

cosine_thm =
  foldl (/\) Verum 
    [function_float_my_cosine2_1]

function_float_my_cosine2_1 = 
  x |>=| - (32769 / 1048576) /\
  x |<=| 32769 / 1048576 /\
  x |>=| - 340282000000000000000000000000000000000 /\
  x |<=| 340282000000000000000000000000000000000 
  --float__size >= 0 .
       --->
  x *: x |>=| - 340282000000000000000000000000000000000 /\
  x *: x |<=| 340282000000000000000000000000000000000
