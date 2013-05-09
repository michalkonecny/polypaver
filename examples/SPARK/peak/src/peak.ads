with PolyPaver.Floats;
--# inherit PolyPaver.Floats;
package Peak is 

	procedure Max (X,Y : in Float; R : out Float) ;
	--# derives R from X,Y;
	--# post (R = X and X >= Y) or (R = Y and Y >= X);
  	
	Y_Min : constant := 0.0 ;
	Y_Max : constant := 1.0 ; -- 100.0 ;
  	
	subtype Iy is Float range Y_Min .. Y_Max ;
  	
	Pad : constant := 0.000001;
  	
	subtype Ipad is Float range -Pad .. Pad ;	  	
	subtype Icoeff1 is Float range 1.1*(Y_Min-Y_Max) .. 1.1*(Y_Max-Y_Min) ;
	subtype Icoeff2 is Float range 1.2*(Y_Min-Y_Max) .. 1.2*(Y_Max-Y_Min) ;
  	
	-- coefficients of quadratic A*X**2+B*X+C over [-1,1] 
	-- interpolating the points (-1,Y1), (0,Y2) and (1,Y3)
	procedure Coeffs (Y1,Y2,Y3 : in Float; A,B,C : out Float);
	--# derives A from Y1,Y2,Y3 &
	--#         B from Y1,Y3 &
	--#         C from Y2 ;
	--# pre  Y1 in Iy and Y2 in Iy and Y3 in Iy;
	--# post A-B+C - Y1 in Ipad and
	--#          C = Y2 and
	--#      A+B+C - Y3 in Ipad and 
	--#      A in Icoeff1 and B in Icoeff1 and C in Icoeff1;
  	
	procedure PeakQ (A,B,C,X : in Float; R : out Float);
	--# derives R from A,B,C,X;
	--# pre  X in -1.0 .. 1.0 and 
	--#      A < -0.05 and 
	--#      A in Icoeff2 and B in Icoeff2 and C in Icoeff2 ;
	--# post R >= A*X**2+B*X+C-0.05 and R <= 10.0;
  	
	procedure PeakUnit (Y1,Y2,Y3 : in Float; R : out Float);
	--# derives R from Y1,Y2,Y3;
	--# pre Y1 in Iy and Y2 in Iy and Y3 in Iy;
	--# post R >= Y1 - 0.2 and R >= Y2 - 0.2 and R >= Y3 - 0.2;

end Peak;
