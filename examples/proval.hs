{-|
    Description :  a few example problems adapted from ProVal 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A few example problems adapted from ProVal.
-}
module Main where

import PolyPaver

main = 
    defaultMain Problem 
        {box = proval_cosine_box
        ,conjecture = proval_cosine_2}
--    defaultMain Problem 
--        {box = proval_sqrt_box
--        ,ivars = []
--        ,theorem = proval_sqrt_3}

a -/ b = (a - b)/b

proval_cosine_box =
    [(0, o2 4),(1, o2 5)]

o2 n = (- d, d)
    where
    d = 2 ^^ (-n)
    
o2e n =
   plusMinus $ Lit (2^^(-n)) 
        
float_range = plusMinus $ Lit float_max
float_max = (fromInteger 0x1FFFFFE) * 2^(127 - 24)

proval_cosine_1 =
    (exact_result3 - Cos exact_x) |<-| (o2e 24)
    where
    float_x = Var 0 "float_x" -- any float, can shrink to eg [-2^(-4),2^(-4)] thanks to the first premise
    exact_x = Var 1 "exact_x" -- [-2^(-5),2^(-5)]

    float_result = 1.0
    float_result0 = (float_x *: float_x);
    float_result1 = 0.5;
    float_result2 = (float_result0 * float_result1); -- division by 2 is exact
    float_result3 = (float_result -: float_result2);

    exact_result = 1.0
    exact_result0 = (exact_x * exact_x);
    exact_result1 = 0.5;
    exact_result2 = (exact_result0 * exact_result1);
    exact_result3 = (exact_result - exact_result2);

proval_cosine_2 =
    ((float_x - exact_x) |<-| (o2e 20)) --->
    ((float_result3 - exact_result3) |<-| plusMinus ((Abs (float_x - exact_x)) + (Lit $ (fromInteger 0x20081) * 2^^(-41)))) -- original 2^^(-41)
    where
    float_x = Var 0 "float_x" -- any float, can shrink to eg [-2^(-4),2^(-4)] thanks to the first premise
    exact_x = Var 1 "exact_x" -- [-2^(-5),2^(-5)]
    
    float_result = 1.0
    float_result0 = (float_x *: float_x);
    float_result1 = 0.5;
    float_result2 = (float_result0 * float_result1); -- division by 2 is exact
    float_result3 = (float_result -: float_result2); -- = 1 - 0.5*float_x*float_x

    exact_result = 1.0
    exact_result0 = (exact_x * exact_x);
    exact_result1 = 0.5;
    exact_result2 = (exact_result0 * exact_result1);
    exact_result3 = (exact_result - exact_result2); -- = 1 - 0.5*exact_x*exact_x
    
proval_cosine_3 =
    ((float_x - exact_x) |<-| (o2e 20)) --->
    (float_x *: float_x) |<-| float_range
    where
    float_x = Var 0 "float_x" -- any float, can shrink to eg [-2^(-4),2^(-4)] thanks to the first premise
    exact_x = Var 1 "exact_x" -- [-2^(-5),2^(-5)]

    float_result = 1.0
    float_result0 = (float_x *: float_x);
    float_result1 = 0.5;
    float_result2 = (float_result0 *: float_result1);
    float_result3 = (float_result -: float_result2);

    exact_result = 1.0
    exact_result0 = (exact_x * exact_x);
    exact_result1 = 0.5;
    exact_result2 = (exact_result0 * exact_result1);
    exact_result3 = (exact_result - exact_result2);

proval_sqrt_box =
    [(0, (0.5,2)),(1, (0.25,4))]

proval_sqrt_3 =
    ((float_result -/ (1.0 / Sqrt(float_x))) |<-| (o2e 6))
    --->
    (float_result6 -/ (1.0 / Sqrt(float_x))) |<-| (o2e 10)
    where
    float_x = Var 0 "float_x" -- any float, can shrink to [0.5,2] thanks to the deleted premises
    float_result = Var 1 "float_result" -- any float, can shrink to eg [0.25,4] thanks to the first premise

    float_result0 = 0.5;
    float_result1 = (float_result0 *: float_result); -- = 0.5r
    float_result2 = 3.0;
    float_result3 = (float_result *: float_result); -- = r^2
    float_result4 = (float_result3 *: float_x);-- = xr^2
    float_result5 = (float_result2 -: float_result4); -- = 3 - xr^2
    float_result6 = (float_result1 *: float_result5); -- 0.5r * (3 - xr^2) 
    


    
