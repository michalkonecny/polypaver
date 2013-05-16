with PolyPaver.Floats;
--# inherit PolyPaver.Exact, PolyPaver.Interval, PolyPaver.Integers, PolyPaver.Floats;
package Riemann is 

function expMinusSquare(x : Float) return Float;
--# pre PolyPaver.Floats.Is_Range(x, -1.0, 5.0);
--# return result =>
--#    PolyPaver.Interval.Contained_In(result, PolyPaver.Exact.Exp(-x*x) + PolyPaver.Interval.Hull(-0.000001,0.000001));

function erfRiemann(x : Float; n : Integer) return Float;
--# pre PolyPaver.Floats.Is_Range(x, 0.0, 4.0)
--#     and PolyPaver.Integers.Is_Range(n, 1, 100);
--# return result => 
--#     PolyPaver.Interval.Contained_In(
--#         result - PolyPaver.Exact.Integral(0.0,x,PolyPaver.Exact.Exp(-PolyPaver.Exact.Integration_Variable**2))
--#         ,
--#         PolyPaver.Interval.Hull(
--#              - 0.1*Float(n+1)
--#              ,
--#              (1.0-PolyPaver.Exact.Exp(-x**2))*x/Float(n)
--#              + 0.1*Float(n+1)
--#         )
--#     );

end Riemann;