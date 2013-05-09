with PolyPaver.Floats;
package body Example is
	
	function Sqrt (X : in Float) return Float
	is
		R,S : Float;
	begin
		S := X;
		R := PolyPaver.Floats.Add(PolyPaver.Floats.Multiply(0.5,X),0.5);
		while R /= s loop
			--# assert R in -0.25*X**2+X .. 0.25*X**2+1.0 ;
			S := r;
			R := PolyPaver.Floats.Multiply(0.5,
				PolyPaver.Floats.Add(S,PolyPaver.Floats.Divide(X,S)));
	   end loop;
	   return R;
	end Sqrt;

end Example;