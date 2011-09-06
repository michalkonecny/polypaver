with numeric;
--# inherit exact, numeric;
--# main_program;
procedure sqrt (x : in float; r : out float)
--# derives r from x;
--# pre  1.0 <= x and x <= 2.0;
--# post r in float and
--#      r <= (1.0+9.0*numeric.epsrel)*exact.sqrt(x);
is
   s : float;
begin
   s := x;
   r := numeric.times(0.5,numeric.plus(x,1.0));
   while abs(numeric.minus(r,s)) > numeric.epsabs loop
      s := r;
      --# assert 0.5 <= s and s <= 3.0;
      r := numeric.times(0.5,numeric.plus(s,numeric.divide(x,s)));
   end loop;
end sqrt;
