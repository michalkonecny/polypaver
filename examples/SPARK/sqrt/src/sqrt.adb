with numeric;
--# inherit exact, numeric;
--# main_program;
procedure sqrt (x : in float; r : out float)
--# derives r from x;
--# pre  1.0 <= x and x <= 2.0;
--# post r in float and
--#      r <= (1.0+4.0*numeric.epsrel)*exact.sqrt(x);
is
   s : float;
begin
   s := x;
   r := numeric.plus(numeric.times(0.5,x),0.5);
   while r /= s loop
      --# assert r in -0.25*x**2+x .. 0.25*x**2+1.0 ;
      s := r;
      r := numeric.times(0.5,numeric.plus(s,numeric.divide(x,s)));
   end loop;
end sqrt;
