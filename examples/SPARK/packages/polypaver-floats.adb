with Ada.Numerics,
     Ada.Numerics.Elementary_Functions;
package body PolyPaver.Floats is

   function Eps_Abs return Float is
   begin
      return 0.5**126;
   end EpsAbs;

   function Eps_Rel return Float is
   begin
      return 0.5**22;
   end EpsRel;

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

   function Exp (X : Float) return Float is
   begin
      return Ada.Numerics.Elementary_Functions.Exp(X);
   end Exp;

end PolyPaver.Floats;
