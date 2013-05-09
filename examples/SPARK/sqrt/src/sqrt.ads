--# inherit PolyPaver.Exact, PolyPaver.Floats;
package Example is
	
	function Sqrt (X : in Float) return Float;
	--# pre  1.0 <= X and X <= 2.0;
	--# return R => 
	--# 	R <= (1.0+4.0*PolyPaver.Floats.Eps_Rel)*PolyPaver.Exact.Sqrt(X);

end Example;
