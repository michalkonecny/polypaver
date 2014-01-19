package PP_LF_Rounded is
   -- Operations whose semantics is the same as the corresponding
   -- operations on type Long_Float.  These operations can appear both in
   -- Ada expressions and in SPARK assertions.
   --
   -- The intention is that programmers do not enter these operations
   -- directly into the Ada code.  Instead the provided pre-processor
   -- replaces each ordinary operator or call of an Long_Elementary_Functions
   -- function with a corresponding function from this package.
   -- 
   -- The Prec parameter is used only by PolyPaver to work
   -- out the rounding precision.  The value for Prec is supplied
   -- by the pre-processor.

    function Plus (Prec : Integer; X,Y : Long_Float) return Long_Float;
    --# pre Plus(Prec,X,Y) in Long_Float;

    function Minus (Prec : Integer; X,Y : Long_Float) return Long_Float;
    --# pre Minus(Prec,X,Y) in Long_Float;

    function Multiply (Prec : Integer; X,Y : Long_Float) return Long_Float;
    --# pre Multiply(Prec,X,Y) in Long_Float;

    function Divide (Prec : Integer; X,Y : Long_Float) return Long_Float;
    --# pre Y /= 0.0 and
    --#     Divide(Prec,X,Y) in Long_Float;

    function Pi(Prec : Integer) return Long_Float;

    function Exp (Prec : Integer; X : Long_Float) return Long_Float;
    --# pre Exp(Prec,X) in Long_Float;
    
    function Sqrt (Prec : Integer; X : Long_Float) return Long_Float;
    --# pre X >= 0.0 and
    --#     Sqrt(Prec,X) in Long_Float;

end PP_LF_Rounded;
