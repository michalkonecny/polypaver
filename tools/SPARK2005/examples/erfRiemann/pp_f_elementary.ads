-- A SPARK wrapper for some elementary functions.
-- The PolyPaver SPARK pre-processor will replace any call
-- of these functions with their equivalent from package
-- PP_F_Rounded, supplying a value for the additional Prec parameter.
package PP_F_Elementary is

   function Pi return Float;

   function Exp (X : Float) return Float;

   function Sqrt (X : Float) return Float;

end PP_F_Elementary;
