with exact;
--# inherit exact;
package numeric is

  function epsabs return float;

  function epsrel return float;

  function plus (x,y : float) return float;
  --# pre plus(x,y) in float;

  function minus (x,y : float) return float;
  --# pre minus(x,y) in float;

  function times (x,y : float) return float;
  --# pre times(x,y) in float;

  function divide (x,y : float) return float;
  --# pre divide(x,y) in float;

end numeric;