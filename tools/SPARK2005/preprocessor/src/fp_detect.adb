with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Asis;
with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Expressions;
with Asis.Exceptions;

package body FP_Detect is

   function Is_PP_Elem_Fn(Element : Asis.Element) return Boolean
   is
      Decl : Asis.Element :=
        Asis.Expressions.Corresponding_Name_Declaration(Element);
      Unit : Asis.Compilation_Unit :=
        Asis.Elements.Enclosing_Compilation_Unit(Decl);
      Unit_Name : Wide_String :=
        Asis.Compilation_Units.Unit_Full_Name(Unit);
   begin
      Put("Unit_Name = ");
      Put(Unit_Name);
      New_Line;
      return
        Unit_Name = "PP_F_Elementary" or
        Unit_Name = "PP_LF_Elementary" or
        Unit_Name = "PP_SF_Elementary";
   exception
      when others =>
         return False;
   end Is_PP_Elem_Fn;

   function Is_ElemFn_Instance(Element : Asis.Element) return Boolean
   is
      Generic_Element : Asis.Element :=
        Asis.Declarations.Corresponding_Generic_Element(Element);
      Generic_Unit : Asis.Compilation_Unit :=
        Asis.Elements.Enclosing_Compilation_Unit(Generic_Element);
      Generic_Unit_Name : Wide_String :=
        Asis.Compilation_Units.Unit_Full_Name(Generic_Unit);
   begin
      return Generic_Unit_Name = "Ada.Numerics.Generic_Elementary_Functions";
   exception
      when others =>
         return False;
   end Is_ElemFn_Instance;


   function Is_FP_Operator_Expression(Element : Asis.Element) return Maybe_FP_Operator
   is
      Root : Asis.Element;
      Type_Info : FP_Type;
      Result_Field_Op : Maybe_FP_Operator(Field_Op);
      Result_Elem_Fn : Maybe_FP_Operator(Elementary_Fn);
   begin
      -- try to obtain FP precision:
      Type_Info := FP_Type_Of(Element);
      -- store the precision in potential result records:
      Result_Field_Op.Type_Info := Type_Info;
      Result_Elem_Fn.Type_Info := Type_Info;

      -- try to extract the root of the expression tree:
      Root := Asis.Expressions.Prefix(Element);
      -- investigate the kind of the root expression:
      case Asis.Elements.Expression_Kind(Root) is
         when Asis.An_Operator_Symbol =>
            -- check whether the operator is a rounded FP operator:
            Result_Field_Op.Op_Kind := Asis.Elements.Operator_Kind(Root);
            case Result_Field_Op.Op_Kind is
               when Asis.A_Divide_Operator
                  | Asis.A_Multiply_Operator
                  | Asis.A_Plus_Operator
                  | Asis.A_Minus_Operator
                  | Asis.An_Exponentiate_Operator =>
                  return Result_Field_Op;
               when others =>
                  return No_FP_Operator;
            end case;
         when Asis.A_Selected_Component =>
            -- check whether the function refers to one of the functions from
            -- Ada.Numerics.Generic_Elementary_Functions:
            if Is_PP_Elem_Fn(Asis.Expressions.Selector(Root)) then
            Result_Elem_Fn.Fn_Name := Asis.Expressions.Selector(Root);
               return Result_Elem_Fn;
            else
               return No_FP_Operator;
            end if;
         when Asis.An_Identifier =>
            if Is_PP_Elem_Fn(Root) then
            Result_Elem_Fn.Fn_Name := Root;
               return Result_Elem_Fn;
            else
               return No_FP_Operator;
            end if;
         when others =>
            return No_FP_Operator;
      end case;
   exception
      when Ex : Asis.Exceptions.ASIS_Inappropriate_Element =>
         return No_FP_Operator;
   end Is_FP_Operator_Expression;

   function FP_Type_Of(Expr : Asis.Element) return FP_Type
   is
      Expr_Type_Decl : Asis.Declaration := Asis.Expressions.Corresponding_Expression_Type(Expr);
      Expr_Type_Def : Asis.Definition := Asis.Declarations.Type_Declaration_View(Expr_Type_Decl);
      Expr_Root_Type_Decl : Asis.Declaration;
      Expr_Root_Type_Def : Asis.Definition;
      Result : FP_Type;
   begin
      -- determine the root type so that we can get its precision:
      begin
         Expr_Root_Type_Decl := Asis.Definitions.Corresponding_Root_Type(Expr_Type_Def);
         Expr_Root_Type_Def := Asis.Declarations.Type_Declaration_View(Expr_Root_Type_Decl);
      exception
         when Ex : others => -- failed to find a root, assume already got the root:
            Expr_Root_Type_Decl := Expr_Type_Decl;
            Expr_Root_Type_Def := Expr_Type_Def;
      end;
      -- extract precision from the root type:
      declare
         Digits_Expr : Asis.Expression := Asis.Definitions.Digits_Expression(Expr_Root_Type_Def);
         Digits_String : Wide_String := Asis.Expressions.Value_Image(Digits_Expr);
      begin
         Result.Precision := Integer'Value(To_String(Digits_String));
      end;

      -- extract the type name:
      Result.Name := Asis.Declarations.Names(Expr_Type_Decl)(1);
      -- check if the name is standard:
      declare
         Name : Wide_String := Asis.Declarations.Defining_Name_Image(Result.Name);
      begin
         if Name = "Float" then
            Result.Is_Standard := True;
            Result.Code := F;
         elsif Name = "Long_Float" then
            Result.Is_Standard := True;
            Result.Code := LF;
         elsif Name = "Short_Float" then
            Result.Is_Standard := True;
            Result.Code := SF;
         else
            Result.Is_Standard := False;
            Result.Code := LF;
         end if;
      end;

      return Result;
   end FP_Type_Of;

end FP_Detect;
