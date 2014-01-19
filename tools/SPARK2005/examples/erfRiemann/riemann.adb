with PP_F_Elementary;
with PP_LF_Elementary;

package body Riemann is

   function exp_Minus_Square(x : MyFloat) return MyFloat
   is
   begin
      return MyFloat(PP_LF_Elementary.Exp(Long_Float(-x*x)));
   end exp_Minus_Square;

   function erf_Riemann(x : Float; n : Integer) return Float
   is
      stepSize : Float;
      stepStart : Float;
      valueStart : Float;
      result : Float;
      step : Integer;
   begin
      stepSize := x/Float(n);
      result := 0.0;
      step := 0;
      while step < n loop -- for step = 0..(n-1) loop
         stepStart := stepSize * Float(step);
         --# assert 
         --#     PP_F_Exact.Contained_In(
         --#         result
         --#         ,
         --#         PP_F_Exact.Integral
         --#            (0.0, stepStart,
         --#             PP_F_Exact.Exp(-PP_F_Exact.Integration_Variable**2)
         --#            )
         --#         +
         --#         PP_F_Exact.Interval(
         --#              - 0.1*Float(step+1)
         --#              ,
         --#              (1.0-PP_F_Exact.Exp(-(x * Float(step)/Float(n))**2))*x/Float(n)
         --#              + 0.1*Float(step+1)
         --#         )
         --#     )
         --#     and PP_F_Exact.Is_Range(result,-10, 100.0)
         --#     and PP_Integer.Is_Range(n, 1, 100) 
         --#     and PP_Integer.Is_Range(step, 0, n-1)
         --#     and stepStart = PP_F_Rounded.Multiply(6, stepSize, Float(step))
         --#     and stepSize = PP_F_Rounded.Divide(6, x, Float(n));

         valueStart :=
           PP_F_Elementary.Exp(-(stepStart * stepStart));
         result := 
           result 
             + stepSize 
           * valueStart;
         step := step + 1;
      end loop;
      return result;
   end erf_Riemann;

end Riemann;
