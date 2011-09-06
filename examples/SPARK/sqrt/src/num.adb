with Ada.Numerics,
     Ada.Numerics.Elementary_Functions;
package body Num is

   function EpsAbs return Float is
   begin
      return 0.5**126;
   end;

   function EpsRel return Float is
   begin
      return 0.5**22;
   end;

   function Pi return Float is
   begin
      return Ada.Numerics.Pi;
   end Pi;

   function Add (X,Y : Float) return Float is
   begin
      return X+Y;
   end Add;

   function Multiply (X,Y : Float) return Float is
   begin
      return X*Y;
   end Multiply;

   function Divide (X,Y : Float) return Float is
   begin
      return X/Y;
   end Divide;

   function Exp (X : Float) return Float is
   begin
      return Ada.Numerics.Elementary_Functions.Exp(X);
   end Exp;

   function Square (X : Float) return Float is
   begin
      return X*X;
   end Square;

   function Cube (X : Float) return Float is
   begin
      return (X*X)*X;
   end Cube;

   function Sqrt (X : Float) return Float is
   begin
      return Ada.Numerics.Elementary_Functions.Sqrt(X);
   end Sqrt;

end Num;
