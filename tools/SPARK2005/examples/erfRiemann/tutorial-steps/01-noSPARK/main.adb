with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Long_Elementary_Functions;

with Riemann;

procedure Main is
   Xs : array(1..4) of Float :=
     (1.0, 2.0, 3.0, 4.0);
   Results : array(1..4) of Long_Float :=
     (0.74682413, 0.8820813907, 0.88620734825952, 0.8862269117895);
   package TIO renames Ada.Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package FIO is new Ada.Text_IO.Float_IO(Float);
   package LFIO is new Ada.Text_IO.Float_IO(Long_Float);
   package LFElem renames Ada.Numerics.Long_Elementary_Functions;
begin
   for XI in Xs'Range loop
      declare
         X : Float := Xs(XI);
         XL : Long_Float := Long_Float(X);
      begin
         for N in 1 .. 10 loop
            declare
               partitionSize : Integer := 2 ** N;
               Res : Long_Float := Long_Float(Riemann.erf_Riemann(X,N));
            begin
               TIO.Put("erf_Riemann(");
               FIO.Put(X, 1, 1, 0);
               TIO.Put(", ");
               IIO.Put(N, 2);
               TIO.Put(") = ");
               LFIO.Put(Res, 1, 6, 0);
               TIO.New_Line;
            end;
         end loop;
         TIO.New_Line;
         TIO.Put("the integral for x = ");
         FIO.Put(X, 1, 1, 0);
         TIO.Put(": ");
         LFIO.Put(Results(XI), 1, 6, 0);
         TIO.New_Line;
         TIO.Put("--------------------------------------------------------");
         TIO.New_Line;
      end;
   end loop;
end Main;
