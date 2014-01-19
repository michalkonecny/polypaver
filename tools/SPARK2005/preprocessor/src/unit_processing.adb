with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.IO_Exceptions;

with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Text;
with Asis.Extensions.Iterator;

with FP_Translation; use FP_Translation;

package body Unit_Processing is

   procedure Recursive_Construct_Processing is new
      Asis.Extensions.Iterator.Traverse_Unit
        (State_Information => FP_Translation.Traversal_State,
         Pre_Operation     => FP_Translation.Pre_Op,
         Post_Operation    => FP_Translation.Post_Op);

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (The_Unit    : Asis.Compilation_Unit;
      Trace       : Boolean := False;
      Output_Path : String)
   is

      Example_Element : Asis.Element := Asis.Elements.Unit_Declaration (The_Unit);
      --  The top-level ctructural element of the library item or subunit
      --  contained in The_Unit.  Only needed as an example element from the
      --  compilation unit so that we can obtain the span of the unit.

      Unit_Span : Asis.Text.Span := Asis.Text.Compilation_Span(Example_Element);
      -- Span is used to indicate a portion of the program text.  Here we need
      -- to span the whole text of the unit as that is the full scope of the translation.

      Unit_Text_Name : String := To_String(Asis.Compilation_Units.Text_Name(The_Unit));
      -- We assume that the text name is the full path of the .adb file
      -- in which the unit is stored.

      Last_Segment_Ix : Integer :=
        Ada.Strings.Fixed.Index
          (Source => Unit_Text_Name,
           Set => Ada.Strings.Maps.To_Set("\\\\/"),
           Going => Ada.Strings.Backward);

      Output_File_Name : String :=
        Output_Path &
        Unit_Text_Name(Last_Segment_Ix..Unit_Text_Name'Last);

      Process_Control : Asis.Traverse_Control := Asis.Continue;
      Process_State   : Traversal_State;
      	-- initialised later because it contains a file handle
   begin

      if Trace then
         Put("Source text name: ");
         Put(To_Wide_String(Unit_Text_Name));
         New_Line;
      end if;

      -- Initialise the transversal state variable,
      -- and open the appropriate output file:
      Process_State.Span := Unit_Span;
      Process_State.Trace := Trace;
      declare
      begin
         Open(Process_State.Output_File, Out_File, Output_File_Name);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Create(Process_State.Output_File, Out_File, Output_File_Name);
      end;

      if Trace then
         Put("Target file name: ");
         Put(To_Wide_String(Output_File_Name));
         New_Line;
      end if;

      -- Write a header to the new file:
      Put(Process_State.Output_File,
          "-- This file has been modified by pp_fpops for floating-point verification.");
      New_Line(Process_State.Output_File);
      Put(Process_State.Output_File,
          "-- pp_fpops is part of PolyPaver (https://github.com/michalkonecny/polypaver).");
      New_Line(Process_State.Output_File);
      Put(Process_State.Output_File,
          "-- Generated on ");
      Put(Process_State.Output_File,
          To_Wide_String(Ada.Calendar.Formatting.Image(Ada.Calendar.Clock)));
      Put(Process_State.Output_File,
                               " from the file:");
      New_Line(Process_State.Output_File);
      Put(Process_State.Output_File,
          "-- ");
      Put(Process_State.Output_File,
          To_Wide_String(Unit_Text_Name));
      New_Line(Process_State.Output_File);
      Put(Process_State.Output_File,
          "-- pp_fpops converted any floating-point operators to calls in the following packages:");
      New_Line(Process_State.Output_File);
      Put(Process_State.Output_File,
          "with PP_SF_Rounded; with PP_F_Rounded; with PP_LF_Rounded;");
      New_Line(Process_State.Output_File);
--        Put(Process_State.Output_File,
--            "--# inherit PP_SF_Rounded, PP_F_Rounded, PP_LF_Rounded;");
--        New_Line(Process_State.Output_File);

      -- Recurse through the unit constructs.
      -- When coming across an FP operator expression,
      -- put all that precedes it and has not been printed yet,
      -- and then put a translation of the FP operator expression.
      Recursive_Construct_Processing
        (Unit => The_Unit,
         Control => Process_Control,
         State   => Process_State);

      -- Put the remainder of the unit body.
      -- If there are no FP operators in the body,
      -- the span is the whole unit body text at the point
      -- and the unit remains unchanged.
      FP_Translation.Put_Current_Span(Example_Element, Process_State);

      -- Close the output file:
      Close(Process_State.Output_File);

   end Process_Unit;

end Unit_Processing;
