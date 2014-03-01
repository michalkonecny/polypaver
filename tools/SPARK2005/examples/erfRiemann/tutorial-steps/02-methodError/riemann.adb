with PP_F_Elementary;

package body Riemann is

   function erf_Riemann(x : Float; n : Integer) return Float
   is
      partitionSize : Integer;
      stepSize : Float;
      tLeft : Float;
      valueLeft : Float;
      result : Float;
      step : Integer;
   begin
      partitionSize := 2 ** n;
      stepSize := x/Float(partitionSize);

      result := 0.0;

      step := 0;
      while step < (2**n) loop
         tLeft := stepSize * Float(step);
         valueLeft := PP_F_Elementary.Exp(-(tLeft * tLeft));
         result := result + stepSize * valueLeft;
         step := step + 1;
      end loop;

      return result;
   end erf_Riemann;

end Riemann;
