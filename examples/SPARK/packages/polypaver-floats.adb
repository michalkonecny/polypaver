with Ada.Numerics,
     Ada.Numerics.Elementary_Functions;
package body PolyPaver.Floats is

   function Eps_Abs return Float is
   begin
      return 0.5**126;
   end Eps_Abs;

   function Eps_Rel return Float is
   begin
      return 0.5**22;
   end Eps_Rel;

   function Pi return Float is
   begin
      return Ada.Numerics.Pi;
   end Pi;

   function Add (X,Y : Float) return Float is
   begin
      return X+Y;
   end Add;

   function Subtract (X,Y : Float) return Float is
   begin
      return X-Y;
   end Subtract;

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

   function Sqrt (X : Float) return Float is
   begin
      return Ada.Numerics.Elementary_Functions.Sqrt(X);
   end Sqrt;

   function Exp (X : Float) return Float is
   begin
      return Ada.Numerics.Elementary_Functions.Exp(X);
   end Exp;

end PolyPaver.Floats;
