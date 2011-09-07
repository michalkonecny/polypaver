
package body peak is

  procedure max (x,y : in Float; r : out Float) is
  begin
    if x >= y then
      r := x;
    else
      r := y;
    end if;
  end max;

  procedure coeffs (y1,y2,y3 : in Float; a,b,c : out Float) is
  begin
    a := -- 0.5*(y1-2*y2+y3)
      numeric.times(
        0.5,
        numeric.plus(
          y1,
          numeric.plus(
            numeric.times(-2.0,y2),
            y3)));
    b := -- 0.5*(y3-y1)
      numeric.times(0.5,numeric.minus(y3,y1));
    c := y2;
  end coeffs;

  procedure peakQ (a,b,c,x : in Float; r : out Float) is
    ghost : Float;
  begin
    ghost := a+(x-x); 
    r :=
      numeric.minus(
        c,
        numeric.divide(
          numeric.times(b,b),
          numeric.times(4.0,ghost)));
  end peakQ;

  procedure peakUnit (y1,y2,y3 : in Float; r : out Float) is
    a,b,c,m1,m2 : Float;
  begin
    max(y1,y3,m1);
    coeffs(y1,y2,y3,a,b,c);
    if a < -0.1 and 2.0*a <= b and b <= -2.0*a then -- poly has peak within [-1,1]
      peakQ(a,b,c,0.0,m2);
      max(m1,m2,r);
    else
      r := m1;
    end if;
  end peakUnit;

end peak;
