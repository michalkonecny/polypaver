package PP_F_Rounded is
   -- Operations whose semantics is the same as the corresponding
   -- operations on type Float.  These operations can appear both in
   -- Ada expressions and in SPARK assertions.
   --
   -- The intention is that programmers do not enter these operations
   -- directly into the Ada code.  Instead the provided pre-processor
   -- replaces each ordinary operator or call of an Elementary_Functions
   -- function with a corresponding function from this package.
   -- 
   -- The Prec parameter is used only by PolyPaver to work
   -- out the rounding precision.  The value for Prec is supplied
   -- by the pre-processor.

    function Plus (Prec : Integer; X,Y : Float) return Float;
    --# pre Plus(Prec,X,Y) in Float;

    function Minus (Prec : Integer; X,Y : Float) return Float;
    --# pre Minus(Prec,X,Y) in Float;

    function Multiply (Prec : Integer; X,Y : Float) return Float;
    --# pre Multiply(Prec,X,Y) in Float;

    function Divide (Prec : Integer; X,Y : Float) return Float;
    --# pre Y /= 0.0 and
    --#     Divide(Prec,X,Y) in Float;

    function Pi(Prec : Integer) return Float;

    function Exp (Prec : Integer; X : Float) return Float;
    --# pre Exp(Prec,X) in Float;
    
    function Sqrt (Prec : Integer; X : Float) return Float;
    --# pre X >= 0.0 and
    --#     Sqrt(Prec,X) in Float;

end PP_F_Rounded;
