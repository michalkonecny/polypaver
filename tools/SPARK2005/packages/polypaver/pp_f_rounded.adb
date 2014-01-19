-- This file cannot be processed by the SPARK Examiner.
with Ada.Numerics,
     Ada.Numerics.Elementary_Functions;
package body PP_F_Rounded is

   function Plus (Prec : Integer; X,Y : Float) return Float is
   begin
      return X+Y;
   end Plus;

   function Minus (Prec : Integer; X,Y : Float) return Float is
   begin
      return X-Y;
   end Minus;

   function Multiply (Prec : Integer; X,Y : Float) return Float is
   begin
      return X*Y;
   end Multiply;

   function Divide (Prec : Integer; X,Y : Float) return Float is
   begin
      return X/Y;
   end Divide;

   function Pi(Prec : Integer) return Float is
   begin
      return Ada.Numerics.Pi;
   end Pi;

   function Sqrt (Prec : Integer; X : Float) return Float is
   begin
      return Ada.Numerics.Elementary_Functions.Sqrt(X);
   end Sqrt;

   function Exp (Prec : Integer; X : Float) return Float is
   begin
      return Ada.Numerics.Elementary_Functions.Exp(X);
   end Exp;

end PP_F_Rounded;
