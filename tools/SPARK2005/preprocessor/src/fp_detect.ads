with Asis;

package FP_Detect is

   type FP_Type_Code is (F, LF, SF);

   type FP_Type is
      record
         Precision   : Positive;
         Name        : Asis.Defining_Name;
         Is_Standard : Boolean;
         Code        : FP_Type_Code;
      end record;

   type FP_Operator_Kind is (Field_Op, Elementary_Fn, Not_An_FP_Operator);

   type Maybe_FP_Operator(Kind : FP_Operator_Kind) is
      record
         case Kind is
            when Field_Op | Elementary_Fn =>
               Type_Info : FP_Type;
               case Kind is
               when Field_Op =>
                  Op_Kind : Asis.Operator_Kinds;
               when Elementary_Fn =>
                  Fn_Name : Asis.Element;
               when Not_An_FP_Operator =>
                  null;
               end case;
            when Not_An_FP_Operator =>
               null;
         end case;
      end record;

   No_FP_Operator : constant Maybe_FP_Operator := (Kind => Not_An_FP_Operator);

   function FP_Type_Of(Expr : Asis.Element) return FP_Type;

   function Is_FP_Operator_Expression(Element : Asis.Element) return Maybe_FP_Operator;

end FP_Detect;
