package Num is

   function EpsAbs return Float;

   function EpsRel return Float;

   function Pi return Float;

   function Add (X,Y : Float) return Float;
  --# pre Add(X,Y) in Float;

   function Subtract (X,Y : Float) return Float;
   --# pre Subtract(X,Y) in Float;

   function Multiply (X,Y : Float) return Float;
   --# pre Multiply(X,Y) in Float;

   function Divide (X,Y : Float) return Float;
   --# pre Y /= 0.0 and
   --#     Divide(X,Y) in Float;

   function Square (X : Float) return Float;
   --# pre Square(X) in Float;

   function Cube (X : Float) return Float;
   --# pre Cube(X) in Float;

   function Sqrt (X : Float) return Float;
   --# pre X >= 0.0 and
   --#     Sqrt(X) in Float;

   function Exp (X : Float) return Float;
   --# pre Exp(X) in Float;

end Num;
