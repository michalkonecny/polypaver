package PP_LF_Exact is
   -- Various SPARK functions for use in SPARK assertions.
   -- These functions are then featured in VCs and PolyPaver will
   -- understand their meaning.  For each of these functions,
   -- its semantics is the exact real interpretation of its name.
   --
   -- The LF in the package name stands for the Long_Float type.
   --
   -- Ideally all occurences of Long_Float in this package would be
   -- replaced with Real and all floating point types would
   -- be subtypes of Real.  
   -- Unfortunately, such type Real is not available in SPARK.
	
    --# function Square (X : Long_Float) return Long_Float;
    --# function Int_Power (X : Long_Float; N : Integer) return Long_Float;
    --# function Sqrt (X : Long_Float) return Long_Float;
    --# function Exp (X : Long_Float) return Long_Float;
    --# function Pi return Long_Float;

    --# function Integral (Lo,Hi,Integrand : Long_Float) return Long_Float;
    --# function Integration_Variable return Long_Float;

    --# function Interval(Low, High : Long_Float) return Long_Float;
    --# function Contained_In(Inner, Outer : Long_Float) return Boolean;

    --# function Is_Range(Variable : Long_Float; Min : Long_Float; Max : Long_Float) return Boolean;
    --# function Eps_Abs(Prec : Integer) return Long_Float;
    --# function Eps_Rel(Prec : Integer) return Long_Float;
    --# function Plus_Minus_Eps_Abs(Prec : Integer) return Long_Float;
    --# function Plus_Minus_Eps_Rel(Prec : Integer) return Long_Float;
   
end PP_LF_Exact;
