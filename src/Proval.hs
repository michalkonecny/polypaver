module Main 
(
  main
)
where

import Paver

main = 
    defaultMain Problem 
        {box = proval_cosine_box
        ,ivars = []
        ,theorem = proval_cosine_2}

proval_cosine_box =
    [(0, o 4),(1, o 5)]

o n = (- d, d)
    where
    d = 2 ^^ (-n)
        
proval_cosine_2 =
    ((float_x - exact_x) |<-| (plusMinus (2^^(-20)))) --->
--    ((float_x *: float_x) |<-| float_range) --->
--    (float_result0 - rnd_ieee_32ne((float_x * float_x))) |<-| [0, 0] --->
--    (exact_result0 - (exact_x * exact_x)) |<-| [0, 0] --->
--    (model_result0 - (model_x * model_x)) |<-| [0, 0] --->
--    ((float_result0 *: float_result1) |<-| float_range) --->
--    (float_result2 - rnd_ieee_32ne((float_result0 * float_result1))) |<-| [0, 0] --->
--    (exact_result2 - (exact_result0 * exact_result1)) |<-| [0, 0] --->
--    (model_result2 - (model_result0 * model_result1)) |<-| [0, 0] --->
--    ((float_result -: float_result2) |<-| float_range) --->
--    (float_result3 - rnd_ieee_32ne((float_result - float_result2))) |<-| [0, 0] --->
--    (exact_result3 - (exact_result - exact_result2)) |<-| [0, 0] --->
--    (model_result3 - (model_result - model_result2)) |<-| [0, 0] --->
--    ((exact_result3 - aux_6) |<-| (plusMinus (2^^(-24)))) --->
--    |(exact_result3 - aux_6)| <= 0x1.p-24 /\
    ((float_result3 - exact_result3) |<-| plusMinus ((Abs (float_x - exact_x)) + ((fromInteger 0x20081) * 2^^(-35)))) -- original 2^^(-41)
    where
    float_x = Var 0 -- any float, can shrink to eg [-2^(-4),2^(-4)] thanks to the first premise
    exact_x = Var 1 -- [-2^(-5),2^(-5)]
    
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
    
float_range = plusMinus float_max
float_max = (fromInteger 0x1FFFFFE) * 2^(127 - 24)

proval_cosine_3 =
    ((float_x - exact_x) |<-| (plusMinus (2^^(-20)))) --->
    (float_x *: float_x) |<-| float_range
    where
    float_x = Var 0 -- any float, can shrink to eg [-2^(-4),2^(-4)] thanks to the first premise
    exact_x = Var 1 -- [-2^(-5),2^(-5)]

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

    
