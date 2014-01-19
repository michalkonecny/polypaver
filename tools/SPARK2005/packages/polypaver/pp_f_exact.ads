package PP_F_Exact is
   -- Various SPARK functions for use in SPARK assertions.
   -- These functions are then featured in VCs and PolyPaver will
   -- understand their meaning.  For each of these functions,
   -- its semantics is the exact real interpretation of its name.
   --
   -- The F in the package name stands for the Float type.
   --
   -- Ideally all occurences of Float in this package would be
   -- replaced with Real and all floating point types would
   -- be subtypes of Real.  
   -- Unfortunately, such type Real is not available in SPARK.
	
    --# function Square (X : Float) return Float;
    --# function Int_Power (X : Float; N : Integer) return Float;
    --# function Sqrt (X : Float) return Float;
    --# function Exp (X : Float) return Float;
    --# function Pi return Float;

    --# function Integral (Lo,Hi,Integrand : Float) return Float;
    --# function Integration_Variable return Float;

    --# function Interval(Low, High : Float) return Float;
    --# function Contained_In(Inner, Outer : Float) return Boolean;

    --# function Is_Range(Variable : Float; Min : Float; Max : Float) return Boolean;
    --# function Eps_Abs(Prec : Integer) return Float;
    --# function Eps_Rel(Prec : Integer) return Float;
    --# function Plus_Minus_Eps_Abs(Prec : Integer) return Float;
    --# function Plus_Minus_Eps_Rel(Prec : Integer) return Float;
   
end PP_F_Exact;
