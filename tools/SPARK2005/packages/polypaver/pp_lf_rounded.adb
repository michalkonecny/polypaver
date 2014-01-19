-- This file cannot be processed by the SPARK Examiner.
with Ada.Numerics,
     Ada.Numerics.Long_Elementary_Functions;
package body PP_LF_Rounded is

   function Plus (Prec : Integer; X,Y : Long_Float) return Long_Float is
   begin
      return X+Y;
   end Plus;

   function Minus (Prec : Integer; X,Y : Long_Float) return Long_Float is
   begin
      return X-Y;
   end Minus;

   function Multiply (Prec : Integer; X,Y : Long_Float) return Long_Float is
   begin
      return X*Y;
   end Multiply;

   function Divide (Prec : Integer; X,Y : Long_Float) return Long_Float is
   begin
      return X/Y;
   end Divide;

   function Pi(Prec : Integer) return Long_Float is
   begin
      return Ada.Numerics.Pi;
   end Pi;

   function Sqrt (Prec : Integer; X : Long_Float) return Long_Float is
   begin
      return Ada.Numerics.Long_Elementary_Functions.Sqrt(X);
   end Sqrt;

   function Exp (Prec : Integer; X : Long_Float) return Long_Float is
   begin
      return Ada.Numerics.Long_Elementary_Functions.Exp(X);
   end Exp;

end PP_LF_Rounded;
