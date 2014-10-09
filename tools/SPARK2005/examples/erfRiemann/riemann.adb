with PP_F_Elementary;

package body Riemann is

   function erf_Riemann(x : Float; n : Integer) return Float
   is
      partitionSize : Integer;
      stepSize : Float;
      stepStart : Float;
      valueStart : Float;
      result : Float;
      step : Integer;
   begin
      partitionSize := 2 ** n;
      stepSize := x/Float(partitionSize);
      result := 0.0;
      step := 0;
      while step < partitionSize loop -- for step = 0..((2^n)-1) loop
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
         --#              - c*Float(step+1)
         --#              ,
         --#              (1.0-PP_F_Exact.Exp(-(x * Float(step)/Float(partitionSize))**2))*x/Float(partitionSize)
         --#              + c*Float(step+1)
         --#         )
         --#     )
         --#     and PP_F_Exact.Is_Range(result,-10, 100.0)
         --#     and PP_Integer.Is_Range(n, 1, 10) 
         --#     and PP_Integer.Is_Range(step, 0, partitionSize-1)
         --#     and partitionSize = 2 ** n
         --#     and stepStart = PP_F_Rounded.Multiply(6, stepSize, Float(step))
         --#     and stepSize = PP_F_Rounded.Divide(6, x, Float(partitionSize));

         valueStart :=
           PP_F_Elementary.Exp(-(stepStart * stepStart));
         result := 
           result + stepSize * valueStart;
         step := step + 1;
      end loop;
      return result;
   end erf_Riemann;

end Riemann;
