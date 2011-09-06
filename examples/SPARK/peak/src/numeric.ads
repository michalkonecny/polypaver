with exact;
--# inherit exact;
package numeric is

  function plus (x,y : float) return float;
  --# pre plus(x,y) in float;

  function minus (x,y : float) return float;
  --# pre minus(x,y) in float;

  function times (x,y : float) return float;
  --# pre times(x,y) in float;

  function divide (x,y : float) return float;
  --# pre divide(x,y) in float;

  function sqrt_init (x : float) return float;
  --# pre  x in 0.5..2.0;
  --# return r => r in (1.0-0.5**6)/exact.sqrt(x)..(1.0+0.5**6)/exact.sqrt(x);

end numeric;