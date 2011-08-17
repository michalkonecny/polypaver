
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
    [function_float_my_cosine2_1
    ,function_float_my_cosine2_2]

function_float_my_cosine2_1 = 
  x |>=| - (32769 / 1048576) /\
  x |<=| 32769 / 1048576 /\
  x |>=| - 340282000000000000000000000000000000000 /\
  x |<=| 340282000000000000000000000000000000000 
  --float__size >= 0 .
       --->
  x *: x |>=| - 340282000000000000000000000000000000000 /\
  x *: x |<=| 340282000000000000000000000000000000000

function_float_my_cosine2_2 = 
  x |>=| - (32769 / 1048576) /\
  x |<=| 32769 / 1048576 /\
  x |>=| - 340282000000000000000000000000000000000 /\
  x |<=| 340282000000000000000000000000000000000 /\
  x *: x |>=| - 340282000000000000000000000000000000000 /\
  x *: x |<=| 340282000000000000000000000000000000000 /\
  x *: x |>=| - 340282000000000000000000000000000000000 /\
  x *: x |<=| 340282000000000000000000000000000000000
  --x *: x |=| x *: x
  --float__size >= 0 .
       --->
  x *: x *: (1 / 2) |>=| - 340282000000000000000000000000000000000 /\
  x *: x *: (1 / 2) |<=| 340282000000000000000000000000000000000

function_float_my_cosine2_3.
H1:    x >= - (32769 / 1048576) .
H2:    x <= 32769 / 1048576 .
H3:    x >= - 340282000000000000000000000000000000000 .
H4:    x <= 340282000000000000000000000000000000000 .
H5:    single__ftimes(x, x) >= - 340282000000000000000000000000000000000 .
H6:    single__ftimes(x, x) <= 340282000000000000000000000000000000000 .
H7:    single__times(x, x) >= - 340282000000000000000000000000000000000 .
H8:    single__times(x, x) <= 340282000000000000000000000000000000000 .
H9:    single__times(x, x) = single__ftimes(x, x) .
H10:   single__ftimes(single__times(x, x), 1 / 2) >= - 
          340282000000000000000000000000000000000 .
H11:   single__ftimes(single__times(x, x), 1 / 2) <= 
          340282000000000000000000000000000000000 .
H12:   single__times(single__times(x, x), 1 / 2) >= - 
          340282000000000000000000000000000000000 .
H13:   single__times(single__times(x, x), 1 / 2) <= 
          340282000000000000000000000000000000000 .
H14:   single__times(single__times(x, x), 1 / 2) = single__ftimes(single__times(
          x, x), 1 / 2) .
H15:   float__size >= 0 .
       ->
C1:    single__fminus(1, single__times(single__times(x, x), 1 / 2)) >= - 
          340282000000000000000000000000000000000 .
C2:    single__fminus(1, single__times(single__times(x, x), 1 / 2)) <= 
          340282000000000000000000000000000000000 .
