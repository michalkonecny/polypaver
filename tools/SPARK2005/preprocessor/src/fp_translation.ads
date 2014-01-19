with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;

with ASIS;
with ASIS.Text;

package FP_Translation is

   type Traversal_State is
      record
         Span : ASIS.Text.Span;
         Output_File : File_Type;
         Trace : Boolean;
      end record;

   procedure Pre_Op
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State);

   procedure Post_Op
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State);

   procedure Put_Current_Span(Element : Asis.Element; State : Traversal_State);

end FP_Translation;
