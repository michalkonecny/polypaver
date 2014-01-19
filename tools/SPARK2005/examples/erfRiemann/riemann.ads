--# inherit PP_Integer, PP_F_Exact, PP_F_Rounded, PP_F_Elementary, PP_LF_Exact, PP_LF_Rounded, PP_LF_Elementary;
package Riemann is 

   type MyFloat is digits 14;
   
function exp_Minus_Square(x : MyFloat) return MyFloat;
--# pre PP_LF_Exact.Is_Range(Long_Float(x), -1.0, 5.0);
--# return result =>
--#    PP_LF_Exact.Contained_In
--#       (Long_Float(result), 
--#        PP_LF_Exact.Exp(Long_Float(-x*x)) + PP_LF_Exact.Interval(-0.000001,0.000001));

function erf_Riemann(x : Float; n : Integer) return Float;
--# pre PP_F_Exact.Is_Range(x, 0.0, 4.0)
--#     and PP_Integer.Is_Range(n, 1, 100);
--# return result => 
--#     PP_F_Exact.Contained_In(
--#         result
--#         ,
--#         PP_F_Exact.Integral(0.0,x,PP_F_Exact.Exp(-PP_F_Exact.Integration_Variable**2))
--#         +
--#         PP_F_Exact.Interval(
--#              - 0.1*Float(n+1)
--#              ,
--#              (1.0-PP_F_Exact.Exp(-x**2))*x/Float(n)
--#              + 0.1*Float(n+1)
--#         )
--#     );

end Riemann;
