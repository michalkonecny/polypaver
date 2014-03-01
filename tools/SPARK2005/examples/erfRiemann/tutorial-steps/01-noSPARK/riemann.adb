with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

package body Riemann is

   function erf_Riemann(x : Float; n : Integer) return Float
   is
      partitionSize : Integer := 2 ** n;
      stepSize : Float := x/Float(partitionSize);
      tLeft : Float;
      valueLeft : Float;
      result : Float;
   begin

      result := 0.0;

      for step in 0..((2**n)-1) loop
         tLeft := stepSize * Float(step);
         valueLeft := Exp(-(tLeft * tLeft));
         result := result + stepSize * valueLeft;
      end loop;

      return result;
   end erf_Riemann;

end Riemann;
