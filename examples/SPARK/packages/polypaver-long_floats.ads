package PolyPaver.Long_Floats is
	
    --# function Is_Range(Variable : Long_Float; Min : Long_Float; Max : Long_Float) return Boolean;

    function EpsAbs return Long_Float;

    function EpsRel return Long_Float;

    function Pi return Long_Float;

    function Add (X,Y : Long_Float) return Long_Float;
   --# pre Add(X,Y) in Long_Float;

    function Subtract (X,Y : Long_Float) return Long_Float;
    --# pre Subtract(X,Y) in Long_Float;

    function Multiply (X,Y : Long_Float) return Long_Float;
    --# pre Multiply(X,Y) in Long_Float;

    function Divide (X,Y : Long_Float) return Long_Float;
    --# pre Y /= 0.0 and
    --#     Divide(X,Y) in Long_Float;

    function Square (X : Long_Float) return Long_Float;
    --# pre Square(X) in Long_Float;

    function Cube (X : Long_Float) return Long_Float;
    --# pre Cube(X) in Long_Float;

    function Sqrt (X : Long_Float) return Long_Float;
    --# pre X >= 0.0 and
    --#     Sqrt(X) in Long_Float;

    function Exp (X : Long_Float) return Long_Float;
    --# pre Exp(X) in Long_Float;
	
end PolyPaver.Long_Floats;
