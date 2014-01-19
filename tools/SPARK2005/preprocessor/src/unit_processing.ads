with Asis;

package Unit_Processing is

   -- Translate all FP operations in the body of a compilation unit
   -- to PolyPaver-friendly function calls.
   procedure Process_Unit (The_Unit    : Asis.Compilation_Unit;
                           Trace       : Boolean := False;
                           Output_Path : String);

end Unit_Processing;
