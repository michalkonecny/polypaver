with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Characters;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Exceptions;

with Asis;
with Asis.Implementation;
with Asis.Exceptions;
with Asis.Errors;
with Asis.Iterator;
with Asis.Expressions;
with Asis.Declarations;
with Asis.Text;

with FP_Detect; use FP_Detect;

package body FP_Translation is

   -- Put the program text defined by the given span within
   -- the compilation unit body that contains the given Element.
   procedure Put_Current_Span
     (Element : Asis.Element; State : Traversal_State)
   is
      Span : constant Asis.Text.Span := State.Span;
   begin
      if not Asis.Text.Is_Nil(Span) then
         declare
            LS : Asis.Text.Line_List := Asis.Text.Lines(Element, Span);
            L1 : Wide_String := Asis.Text.Line_Image(LS(LS'First));
         begin
            Put(State.Output_File, L1(Span.First_Column .. L1'Last));
            if Span.First_Line < Span.Last_Line then
               New_Line(State.Output_File);
               for I in LS'First+1 .. LS'Last-1 loop
                  Put(State.Output_File, Asis.Text.Line_Image(LS(I)));
                  New_Line(State.Output_File);
               end loop;
               Put(State.Output_File, Asis.Text.Line_Image(LS(LS'Last)));
            end if;
         end;
      end if;
   end Put_Current_Span;

   function Spans_Multiple_Lines (Expr : Asis.Element) return Boolean is
      Expr_Span : Asis.Text.Span := Asis.Text.Element_Span(Expr);
   begin
      return Expr_Span.First_Line < Expr_Span.Last_Line;
   end Spans_Multiple_Lines;

   procedure Put_Prefix
     (FP_Operator : Maybe_FP_Operator;
      State : Traversal_State;
      Is_Multi_Line : Boolean;
      First_Column : Positive)
   is
      Type_Info : FP_Type := FP_Operator.Type_Info;
   begin
      -- add type conversion if the type is non-standard:
      if not Type_Info.Is_Standard then
	Put(State.Output_File, Asis.Declarations.Defining_Name_Image(Type_Info.Name));
      end if;
      Put(State.Output_File, "(PP_");
      Put(State.Output_File, FP_Type_Code'Wide_Image(FP_Operator.Type_Info.Code));
      Put(State.Output_File, "_Rounded.");
      case FP_Operator.Kind is
         when Field_Op =>
            case FP_Operator.Op_Kind is
            when Asis.A_Plus_Operator => Put(State.Output_File, "Plus");
            when Asis.A_Minus_Operator => Put(State.Output_File, "Minus");
            when Asis.A_Multiply_Operator => Put(State.Output_File, "Multiply");
            when Asis.A_Divide_Operator => Put(State.Output_File, "Divide");
            when Asis.An_Exponentiate_Operator => Put(State.Output_File, "Exponentiate");
            when others => Put(State.Output_File, "UNKNOWN_FP_OP");
            end case;
         when Elementary_Fn =>
            Put(State.Output_File,  Asis.Expressions.Name_Image(FP_Operator.Fn_Name));
         when Not_An_FP_Operator =>
            Put(State.Output_File, "UNKNOWN_FP_OP");
      end case;
      Put(State.Output_File, "(");
      Put(State.Output_File, To_Wide_String(Integer'Image(FP_Operator.Type_Info.Precision)));
   end Put_Prefix;

   procedure Recursive_Construct_Processing is new
      Asis.Iterator.Traverse_Element
        (State_Information => Traversal_State,
         Pre_Operation     => Pre_Op,
         Post_Operation    => Post_Op);

   procedure Put_Params
     (Params_Association : Asis.Association_List;
      Is_Standard_Type : Boolean;
      State : in out Traversal_State;
      Is_Multi_Line : Boolean;
      First_Column : Positive)
   is
      Param : Asis.Element;
   begin
      for I in Params_Association'Range loop
         Param := Asis.Expressions.Actual_Parameter(Params_Association(I));
         declare
            Process_Control : Asis.Traverse_Control := Asis.Continue;
         begin
            -- print parameter separator, possibly a multi-line one:
            if Is_Multi_Line then
               New_Line(State.Output_File);
               for I in 2..First_Column loop Put(State.Output_File, " "); end loop;
            end if;
            Put(State.Output_File, ", ");

            -- optionally put type conversion if the type is non-standard:
            if not Is_Standard_Type then
                Put(State.Output_File, " Long_Float");
            end if;
            Put(State.Output_File, "(");

            -- traverse the actual parameter expression,
            -- translating FP operators within it:
            State.Span := Asis.Text.Element_Span(Param);
            Recursive_Construct_Processing
              (Element => Param,
               Control => Process_Control,
               State   => State);

            -- print the remainder of the parameter expression:
            Put_Current_Span(Param, State);
            Put(State.Output_File, ")");
         end;
      end loop;
   end Put_Params;

   procedure Split_Span
     (orig, sub : in Asis.Text.Span;
      left, right : out Asis.Text.Span)
   is
   begin
         left :=
           (First_Line => orig.First_Line,
            First_Column => orig.First_Column,
            Last_Line => sub.First_Line,
            Last_Column => sub.First_Column - 1);
         right :=
           (First_Line => sub.Last_Line,
            First_Column => sub.Last_Column + 1,
            Last_Line => orig.Last_Line,
            Last_Column => orig.Last_Column);
   end Split_Span;

   procedure Put_Span_Description(Span : Asis.Text.Span) is
   begin
      Put("(");
      Put(To_Wide_String(Integer'Image(Span.First_Line)));
      Put(":");
      Put(To_Wide_String(Integer'Image(Span.First_Column)));
      Put(")..(");
      Put(To_Wide_String(Integer'Image(Span.Last_Line)));
      Put(":");
      Put(To_Wide_String(Integer'Image(Span.Last_Column)));
      Put(")");
   end Put_Span_Description;

   procedure Pre_Op
     (Element : Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State)
   is
      This_FP_Operator : Maybe_FP_Operator := Is_FP_Operator_Expression(Element);
      Is_Multi_Line : Boolean := Spans_Multiple_Lines(Element);

   begin
      if This_FP_Operator.Kind /= Not_An_FP_Operator then
         -- do not traverse the sub-expressions:
         Control := Asis.Abandon_Children;

         if State.Trace then
            Put("Processing FP expression at ");
            Put_Span_Description(Asis.Text.Element_Span(Element));
            New_Line;
            Put("--------");
            New_Line;
            Put(Asis.Text.Element_Image(Element));
            New_Line;
            Put("--------");
            New_Line;
         end if;

         declare
            e_span : Asis.Text.Span := Asis.Text.Element_Span(Element);
            left, right : Asis.Text.Span;
         begin
            Split_Span(State.Span, e_span, left, right);
            -- put everything up to this expression:
            State.Span := left;
            Put_Current_Span(Element, State);
            Put_Prefix(This_FP_Operator,
                       State,
                       Is_Multi_Line,
                       e_span.First_Column);
            Put_Params(Asis.Expressions.Function_Call_Parameters(Element),
                       This_FP_Operator.Type_Info.Is_Standard,
                       State,
                       Is_Multi_Line,
                       e_span.First_Column);
            Put(State.Output_File, "))");
            -- only the "right" span now remains to be processed and put:
            State.Span := right;
         end;
      end if;
   exception

      when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
           Asis.Exceptions.ASIS_Inappropriate_Container        |
           Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
           Asis.Exceptions.ASIS_Inappropriate_Element          |
           Asis.Exceptions.ASIS_Inappropriate_Line             |
           Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
           Asis.Exceptions.ASIS_Failed                         =>

         Ada.Wide_Text_IO.Put ("Pre_Op : ASIS exception (");

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

         Ada.Wide_Text_IO.Put ("Pre_Op : ");

         Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
                               Ada.Exceptions.Exception_Name (Ex)));

         Ada.Wide_Text_IO.Put (" is raised (");

         Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
                               Ada.Exceptions.Exception_Information (Ex)));

         Ada.Wide_Text_IO.Put (")");
         Ada.Wide_Text_IO.New_Line;
   end Pre_Op;

   procedure Post_Op
     (Element : Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State)
   is
   begin
      null;
   end Post_Op;

end FP_Translation;
