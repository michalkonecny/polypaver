--# inherit PP_F_Exact, PP_F_Elementary;
package Riemann is 

   -- An approximation of the function erf(x)*pi/2, which is equal
   -- to the integral \int_0^x(e^(-t^2))dt.  This integral does not
   -- have a closed algebraic solution.  This function uses a simple
   -- Riemann sum to approximate the integral.
   -- 
   -- The parameter n determines the number of segments to be used in the
   -- partition of equal size.  There are 2^n segments.
   function erf_Riemann(x : Float; n : Integer) return Float;
   --# return result => 
   --#     PP_F_Exact.Contained_In(
   --#         result
   --#         ,
   --#         PP_F_Exact.Integral(0.0,x,PP_F_Exact.Exp(-PP_F_Exact.Integration_Variable**2))
   --#         +
   --#         PP_F_Exact.Interval(
   --#              0.0
   --#              ,
   --#              (1.0-PP_F_Exact.Exp(-x**2))*x/Float(2**n)
   --#         )
   --#     );

end Riemann;
