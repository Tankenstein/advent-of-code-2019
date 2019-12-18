with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Containers.Indefinite_Hashed_Maps;

procedure Main is
   package IO renames Ada.Text_IO;
   package Strings renames Ada.Strings;

   package Orbits_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type => String,
       Element_Type => String,
       Hash => Ada.Strings.Hash_Case_Insensitive,
       Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   Center_Of_Mass : aliased constant String := "COM";

   function Parse_Orbits_From_File (Name: String) return Orbits_Maps.Map is
      File: IO.File_Type;
      Orbits: Orbits_Maps.Map;
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

   function Number_Of_Steps (Orbits: Orbits_Maps.Map; Source: String; Target: String) return Integer is
      package SU renames Ada.Strings.Unbounded;
      Steps : Integer := 1;
      Current_Node : SU.Unbounded_String := SU.To_Unbounded_String (Source);
   begin
      while SU.To_String (Current_Node) /= Target Loop
         Steps := Steps + 1;
         Current_Node := SU.To_Unbounded_String (Orbits.Element (SU.To_String (Current_Node)));
      end Loop;
      return Steps;
   end Number_Of_Steps;

   function Count_Orbits (Orbits: Orbits_Maps.Map) return Integer is
      Orbit_Count : Integer := 0;
      Orbiter_Cursor : Orbits_Maps.Cursor := Orbits_Maps.First (Orbits);
   begin
      while Orbits_Maps.Has_Element (Orbiter_Cursor) Loop
         declare
            Current_Orbiter : String := Orbits_Maps.Element (Orbiter_Cursor);
            Orbits_From_Center : Integer := Number_Of_Steps
               (Orbits => Orbits,
                Source => Current_Orbiter,
                Target => Center_Of_Mass);
         begin
            Orbit_Count := Orbit_Count + Orbits_From_Center;
            Orbits_Maps.Next (Orbiter_Cursor);
         end;
      end Loop;
      return Orbit_Count;
   end Count_Orbits;

   Input_File_Name : aliased constant String := "simple";
begin
   declare
      Orbits: Orbits_Maps.Map := Parse_Orbits_From_File (Ada.Command_Line.Argument (1));
      Orbit_Count : Integer := Count_Orbits (Orbits);
   begin
      IO.Put_Line (Integer'Image (Orbit_Count));
   end;
end Main;

