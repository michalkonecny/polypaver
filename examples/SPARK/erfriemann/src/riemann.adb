package body Riemann is

function erfRiemann(x : Float; n : Integer) return Float
is
 stepSize : Float;
 stepStart : Float;
 valueStart : Float;
 result : Float;
 step : Integer;
begin
 stepSize := PolyPaver.Floats.Divide(x,Float(n));
 result := 0.0;
 step := 0;
 while step < n loop -- for step = 0..(n-1) loop
   stepStart := 
     -- step * stepSize;
     PolyPaver.Floats.Multiply(stepSize, Float(step));
   --# assert 
   --#     PolyPaver.Interval.Contained_In(
   --#         result - PolyPaver.Exact.Integral(0.0,stepStart,PolyPaver.Exact.Exp(-PolyPaver.Exact.Integration_Variable**2))
   --#         ,
   --#         PolyPaver.Interval.Hull(
   --#              - 0.1*Float(step+1)
   --#              ,
   --#              (1.0-PolyPaver.Exact.Exp(-(x * Float(step)/Float(n))**2))*x/Float(n)
   --#              + 0.1*Float(step+1)
   --#         )
   --#     )
   --#     and PolyPaver.Floats.Is_Range(result,-10, 100.0)
   --#     and PolyPaver.Integers.Is_Range(n, 1, 100) and PolyPaver.Integers.Is_Range(step, 0, n-1)
   --#     and stepStart = PolyPaver.Floats.Multiply(stepSize, Float(step))
   --#     and stepSize = PolyPaver.Floats.Divide(x,Float(n));

   valueStart := 
     -- valueStart := exp(-(stepStart * stepStart));
     PolyPaver.Floats.Exp(-PolyPaver.Floats.Multiply(stepStart,stepStart)); 
   result := 
     -- result + stepSize * valueStart;
     PolyPaver.Floats.Add(result,PolyPaver.Floats.Multiply(stepSize,valueStart));
   step := step + 1;
 end loop;
 return result;
end erfRiemann;

end Riemann;