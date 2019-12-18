with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;

procedure Solution_2 is
   package IO renames Ada.Text_IO;
   package Strings renames Ada.Strings;

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type => String,
       Element_Type => String,
       Hash => Ada.Strings.Hash_Case_Insensitive,
       Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Natural,
       Element_Type => String);

   Center_Of_Mass : aliased constant String := "COM";
   You : aliased constant String := "YOU";
   Santa : aliased constant String := "SAN";
   Not_Found : exception;

   function Parse_Orbits_From_File (Name: String) return String_Maps.Map is
      File: IO.File_Type;
      Orbits: String_Maps.Map;
   begin
      IO.Open (File => File, Mode => IO.In_File, Name => Name);
      While not IO.End_Of_File (File) Loop
         declare
            Line : String := IO.Get_Line (File);
            Paren_Index : Integer := Strings.Fixed.Index(Line, ")", 1);
            Orbit_Target : String := Line(Line'First..Line'First + Paren_Index - 2);
            Orbiter : String := Line(Line'First + Paren_Index..Line'Last);
         begin
            Orbits.Insert(Orbiter, Orbit_Target);
         end;
      end loop;
      IO.Close (File);
      return Orbits;
   end Parse_Orbits_From_File;

   function Build_Steps (Orbits: String_Maps.Map; Source: String; Target: String) return String_Vectors.Vector is
      package SU renames Ada.Strings.Unbounded;
      Current_Node : SU.Unbounded_String := SU.To_Unbounded_String (Source);
      Steps : String_Vectors.Vector;
   begin
      while SU.To_String (Current_Node) /= Target Loop
         Current_Node := SU.To_Unbounded_String (Orbits.Element (SU.To_String (Current_Node)));
         Steps.Append(SU.To_String (Current_Node));
      end Loop;
      return Steps;
   end Build_Steps;

   function Find_First_Intersection (First_Vector: String_Vectors.Vector; Second_Vector: String_Vectors.Vector) return String is
      Cursor : String_Vectors.Cursor := String_Vectors.First (First_Vector);
   begin
      while String_Vectors.Has_Element (Cursor) loop
         declare
            Element : String := String_Vectors.Element (Cursor);
            Index_In_Second : Integer := Second_Vector.Find_Index (Element);
         begin
            if Index_In_Second /= String_Vectors.No_Index then
               return Element;
            end if;
            String_Vectors.Next (Cursor);
         end;
      end loop;
      raise Not_Found;
   end Find_First_Intersection;
begin
   declare
      Orbits: String_Maps.Map := Parse_Orbits_From_File (Ada.Command_Line.Argument (1));
      You_Steps : String_Vectors.Vector := Build_Steps (Orbits, You, Center_Of_Mass);
      Santa_Steps : String_Vectors.Vector := Build_Steps (Orbits, Santa, Center_Of_Mass);
      Intersection : String := Find_First_Intersection (You_Steps, Santa_Steps);
      You_Intersection_Index : Integer := You_Steps.Find_Index (Intersection);
      Santa_Intersection_Index : Integer := Santa_Steps.Find_Index (Intersection);
   begin
      IO.Put_Line (Integer'Image (You_Intersection_Index + Santa_Intersection_Index));
   end;
end Solution_2;

