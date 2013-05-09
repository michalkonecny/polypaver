
package body Peak is

	function Max (X,Y : Float) return Float is
		R : Float;
	begin
		if X >= Y then
			R := X;
		else
			R := Y;
		end if;
		return R;
	end Max;
  	
	procedure Coeffs (Y1,Y2,Y3 : in Float; A,B,C : out Float) is
	begin
		A := -- 0.5*(Y1-2*Y2+Y3)
			PolyPaver.Floats.Multiply(
				0.5,
				PolyPaver.Floats.Add(
					Y1,
					PolyPaver.Floats.Add(
						PolyPaver.Floats.Multiply(-2.0,Y2),
						Y3)));
		B := -- 0.5*(Y3-Y1)
			PolyPaver.Floats.Multiply(0.5,PolyPaver.Floats.Add(y3,-y1));
		C := Y2;
	end Coeffs;
  	
	function PeakQ (A,B,C,X : in Float) return Float
	is
		Ghost : Float;
	begin
		Ghost := 
			PolyPaver.Floats.Add(
				A,
				PolyPaver.Floats.Add(X,-X)); 
		return
			PolyPaver.Floats.Add(
				C,
	  			-PolyPaver.Floats.Divide(
	  				PolyPaver.Floats.Multiply(B,B),
	  				PolyPaver.Floats.Multiply(4.0,Ghost)));
	end PeakQ;
  	
	function PeakUnit (Y1,Y2,Y3 : Float) return Float
	is
		A,B,C,M1,M2,R : Float;
	begin
		M1 := Max(Y1,Y3);
		Coeffs(Y1,Y2,Y3,A,B,C);
		if A < -0.05 
			and PolyPaver.Floats.Multiply(2.0,A) <= B 
			and	B <= PolyPaver.Floats.Multiply(-2.0,A) 
		then -- poly has peak within [-1,1]
			R := Max(M1,PeakQ(A,B,C,0.0));
		else
			R := M1;
		end if;
		return R;
	end PeakUnit;

end Peak;
