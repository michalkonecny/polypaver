
package body Peak is

	procedure Max (X,Y : in Float; R : out Float) 
	is
	begin
		if X >= Y then
			R := X;
		else
			R := Y;
		end if;
	end max;
  	
	procedure Coeffs (Y1,Y2,Y3 : in Float; A,B,C : out Float) 
	is
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
  	
	procedure PeakQ (A,B,C,X : in Float; R : out Float) 
	is
		Ghost : Float;
	begin
		Ghost := 
			PolyPaver.Floats.Add(
				A,
				PolyPaver.Floats.Add(X,-X)); 
		R :=
			PolyPaver.Floats.Add(
				C,
	  			-PolyPaver.Floats.Divide(
	  				PolyPaver.Floats.Multiply(B,B),
	  				PolyPaver.Floats.Multiply(4.0,Ghost)));
	end peakQ;
  	
	procedure PeakUnit (Y1,Y2,Y3 : in Float; R : out Float) 
	is
		A,B,C,M1,M2 : Float;
	begin
		Max(Y1,Y3,M1);
		Coeffs(Y1,Y2,Y3,A,B,C);
		if A < -0.05 
			and PolyPaver.Floats.Multiply(2.0,A) <= B 
			and	B <= -2.0*A 
		then -- poly has peak within [-1,1]
			peakQ(A,B,C,0.0,M2);
			max(M1,M2,R);
		else
			r := m1;
		end if;
	end PeakUnit;

end Peak;
