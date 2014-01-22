-- This file cannot be processed by the SPARK Examiner.
with Ada.Numerics,
     Ada.Numerics.Long_Elementary_Functions;
package body PP_LF_Elementary is

   function Pi return Long_Float is
   begin
      return Ada.Numerics.Pi;
   end Pi;

   function Sqrt (X : Long_Float) return Long_Float
     renames Ada.Numerics.Long_Elementary_Functions.Sqrt;

   function Exp (X : Long_Float) return Long_Float
     renames Ada.Numerics.Long_Elementary_Functions.Exp;

end PP_LF_Elementary;
