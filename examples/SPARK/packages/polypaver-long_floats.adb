with Ada.Numerics,
     Ada.Numerics.Generic_Elementary_Functions;
package body PolyPaver.Long_Floats is
   package Long_Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Long_Float);

   function Eps_Abs return Long_Float is
   begin
      return 0.5**126;
   end Eps_Abs;

   function Eps_Rel return Long_Float is
   begin
      return 0.5**22;
   end Eps_Rel;

   function Pi return Long_Float is
   begin
      return Ada.Numerics.Pi;
   end Pi;

   function Add (X,Y : Long_Float) return Long_Float is
   begin
      return X+Y;
   end Add;

   function Subtract (X,Y : Long_Float) return Long_Float is
   begin
      return X-Y;
   end Subtract;

   function Multiply (X,Y : Long_Float) return Long_Float is
   begin
      return X*Y;
   end Multiply;

   function Divide (X,Y : Long_Float) return Long_Float is
   begin
      return X/Y;
   end Divide;

   function Square (X : Long_Float) return Long_Float is
   begin
      return X*X;
   end Square;

   function Sqrt (X : Long_Float) return Long_Float is
   begin
      return Long_Float_Functions.Sqrt(X);
   end Sqrt;

   function Exp (X : Long_Float) return Long_Float is
   begin
      return Long_Float_Functions.Exp(X);
   end Exp;

end PolyPaver.Long_Floats;
