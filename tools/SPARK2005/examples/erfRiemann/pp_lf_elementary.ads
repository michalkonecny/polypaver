-- A SPARK wrapper for some elementary functions.
-- The PolyPaver SPARK pre-processor will replace any call
-- of these functions with their equivalent from package
-- PP_LF_Rounded, supplying a value for the additional Prec parameter.
package PP_LF_Elementary is

   function Pi return Long_Float;

   function Exp (X : Long_Float) return Long_Float;

   function Sqrt (X : Long_Float) return Long_Float;

end PP_LF_Elementary;
