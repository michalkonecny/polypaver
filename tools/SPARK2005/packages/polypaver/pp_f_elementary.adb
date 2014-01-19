-- This file cannot be processed by the SPARK Examiner.
with Ada.Numerics,
     Ada.Numerics.Elementary_Functions;
package body PP_F_Elementary is

   function Pi return Float is
   begin
      return Ada.Numerics.Pi;
   end Pi;

   function Sqrt (X : Float) return Float
     renames Ada.Numerics.Elementary_Functions.Sqrt;

   function Exp (X : Float) return Float
     renames Ada.Numerics.Elementary_Functions.Exp;

end PP_F_Elementary;
