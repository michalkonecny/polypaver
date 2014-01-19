with Ada.Exceptions;
with Ada.Wide_Text_IO;
use Ada.Wide_Text_IO;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;

with Asis;
with Asis.Ada_Environments;
with Asis.Implementation;
with Asis.Exceptions;
with Asis.Errors;

with Context_Processing;

procedure PP_FPops is
   My_Context            : Asis.Context;

   My_Context_Name       : Wide_String := Asis.Ada_Environments.Default_Name;
   --  The default name in case of the GNAT ASIS implementation is empty.
   --  In the GNAT ASIS implementation the name of a Context does not have any
   --  special meaning or semantics associated with particular names.

   My_Context_Parameters : Wide_String :=
      Asis.Ada_Environments.Default_Parameters;
   --  The default Context parameters in case of the GNAT ASIS implementation
   --  are an empty string. This corresponds to the following Context
   --  definition: "-CA -FT -SA", and has the following meaning:
   --  -CA - a Context is made up by all the tree files in the tree search
   --        path; the tree search path is not set, so the default is used,
   --        and the default is the current directory;
   --  -FT - only pre-created trees are used, no tree file can be created by
   --        ASIS;
   --  -SA - source files for all the Compilation Units belonging to the
   --        Context (except the predefined Standard package) are considered
   --        in the consistency check when opening the Context;


   Initialization_Parameters : Wide_String := "";
   Finalization_Parameters   : Wide_String := "";
   --  If you would like to use some specific initialization or finalization
   --  parameters, you may set them here as initialization expressions in
   --  the declarations above.

   -- Function that processes the mandatory argument and outputs an error message
   -- if the argument is not there or is not appropriate.
   function Get_Output_Path return String is
      package C renames Ada.Command_Line;
      package D renames Ada.Directories;
   begin
      if C.Argument_Count = 1 then
         declare
            Argument : String := C.Argument(1);
            Full_Path : String := D.Full_Name(Argument);
         begin
            case D.Kind(Full_Path) is
            when D.Directory =>
               if Full_Path /= D.Full_Name(D.Current_Directory) then
                  -- success!
                  return Full_Path;
               else
                  Put("The argument directory must be distinct from the current directory.");
                  New_Line;
               end if;
            when others =>
               Put("The argument ");
               Put(To_Wide_String(Argument));
               Put(" is not a directory.");
               New_Line;
            end case;
         exception
            when D.Name_Error =>
               Put("The argument ");
               Put(To_Wide_String(Argument));
               Put(" is not an existing directory.");
               New_Line;
         end;
      else
         Put("There has to be exactly one command-line argument: <Output_Directory>");
         New_Line;
      end if;
      -- if we got here, we failed:
      return "";
   end Get_Output_Path;

   Output_Path : String := Get_Output_Path;
begin
   -- Check that there is one argument and check it is an existing file path:
   if Output_Path /= "" then

      --  Initialise ASIS context:
      Asis.Implementation.Initialize     (Initialization_Parameters);

      Asis.Ada_Environments.Associate
        (The_Context => My_Context,
         Name        => My_Context_Name,
         Parameters  => My_Context_Parameters);

      Asis.Ada_Environments.Open         (My_Context);

      -- Do the translation:
      Context_Processing.Process_Context (The_Context => My_Context,
                                          Trace       => True,
                                          Output_Path => Output_Path);

      --  Finalise ASIS context:
      Asis.Ada_Environments.Close        (My_Context);
      Asis.Ada_Environments.Dissociate   (My_Context);
      Asis.Implementation.Finalize       (Finalization_Parameters);
   end if;
exception
   --  The exception handling in this driver is somewhat redundant and may
   --  need some reconsidering when using this driver in real ASIS tools

   when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
             Asis.Exceptions.ASIS_Inappropriate_Container        |
             Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
             Asis.Exceptions.ASIS_Inappropriate_Element          |
             Asis.Exceptions.ASIS_Inappropriate_Line             |
             Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
             Asis.Exceptions.ASIS_Failed                         =>

      Ada.Wide_Text_IO.Put ("ASIS exception (");
      Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Name (Ex)));
      Ada.Wide_Text_IO.Put (") is raised");
      Ada.Wide_Text_IO.New_Line;

      Ada.Wide_Text_IO.Put ("ASIS Error Status is ");
      Ada.Wide_Text_IO.Put
        (Asis.Errors.Error_Kinds'Wide_Image (Asis.Implementation.Status));
      Ada.Wide_Text_IO.New_Line;

      Ada.Wide_Text_IO.Put ("ASIS Diagnosis is ");
      Ada.Wide_Text_IO.New_Line;
      Ada.Wide_Text_IO.Put (Asis.Implementation.Diagnosis);
      Ada.Wide_Text_IO.New_Line;

      Asis.Implementation.Set_Status;

   when Ex : others =>

      Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Name (Ex)));
      Ada.Wide_Text_IO.Put (" is raised (");
      Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Information (Ex)));
      Ada.Wide_Text_IO.Put (")");
      Ada.Wide_Text_IO.New_Line;

end PP_FPops;
