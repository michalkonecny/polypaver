with PolyPaver.Floats;
--# inherit PolyPaver.Exact, PolyPaver.Floats;
--# main_program;
procedure Sqrt (X : in Float; R : out Float)
--# derives R from X;
--# pre  1.0 <= X and X <= 2.0;
--# post R in Float and
--#      R <= (1.0+4.0*PolyPaver.Floats.Eps_Rel)*PolyPaver.Exact.Sqrt(X);
is
	S : Float;
begin
	S := x;
	R := PolyPaver.Floats.Plus(PolyPaver.Floats.Times(0.5,X),0.5);
	while R /= s loop
		--# assert R in -0.25*X**2+X .. 0.25*X**2+1.0 ;
		S := r;
		R := PolyPaver.Floats.Times(0.5,
			PolyPaver.Floats.Plus(S,PolyPaver.Floats.Divide(X,S)));
   end loop;
end Sqrt;
