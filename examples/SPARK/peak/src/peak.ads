with numeric;
--# inherit numeric;
package peak is 

  procedure max (x,y : in Float; r : out Float) ;
  --# derives r from x,y;
  --# post (r = x and x >= y) or (r = y and y >= x);

  y_min : constant := 0.0 ;
  y_max : constant := 1.0 ; -- 100.0 ;

  subtype Iy is Float range y_min .. y_max ;

  pad : constant := 0.000001;

  subtype Ipad is Float range -pad .. pad ;

  subtype Icoeff1 is Float range 1.1*(y_min-y_max) .. 1.1*(y_max-y_min) ;
  subtype Icoeff2 is Float range 1.2*(y_min-y_max) .. 1.2*(y_max-y_min) ;

  -- coefficients of quadratic a*x**2+b*x+c over [-1,1] 
  -- interpolating the points (-1,y1), (0,y2) and (1,y3)
  procedure coeffs (y1,y2,y3 : in Float; a,b,c : out Float);
  --# derives a from y1,y2,y3 &
  --#         b from y1,y3 &
  --#         c from y2 ;
  --# pre  y1 in Iy and y2 in Iy and y3 in Iy;
  --# post a-b+c - y1 in Ipad and
  --#          c = y2 and
  --#      a+b+c - y3 in Ipad and 
  --#      a in Icoeff1 and b in Icoeff1 and c in Icoeff1;

  procedure peakQ (a,b,c,x : in Float; r : out Float);
  --# derives r from a,b,c,x;
  --# pre  x in -1.0 .. 1.0 and 
  --#      a < -0.05 and 
  --#      a in Icoeff2 and b in Icoeff2 and c in Icoeff2 ;
  --# post r >= a*x**2+b*x+c-0.05 and r <= 10.0;

  procedure peakUnit (y1,y2,y3 : in Float; r : out Float);
  --# derives r from y1,y2,y3;
  --# pre y1 in Iy and y2 in Iy and y3 in Iy;
  --# post r >= y1 - 0.2 and r >= y2 - 0.2 and r >= y3 - 0.2;

end peak;
