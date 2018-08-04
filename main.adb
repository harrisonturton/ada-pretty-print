with Ada.Text_IO;            use Ada.Text_IO;
with Pretty_Print; 
with Ada.Numerics.discrete_Random;

procedure Main is
  No_Of_Tasks : constant Positive := 3;
  subtype Task_Range is Positive range 1 .. No_Of_Tasks;

  -- Must import generic package *after* defining
  -- how many tasks to use.
  package Task_Print is new Pretty_Print (No_Of_Tasks);
  use Task_Print;

  subtype Rand_Range is Positive;
  package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
  use Rand_Int;
  gen : Rand_Int.Generator;

  task type Writer is
    entry Handover_Id (Given_Id : Task_Range);
  end Writer;

  task body Writer is
    Id : Task_Range;
  begin
    Put_Line("Unknown task waiting for ID...");
    accept Handover_Id (Given_Id : Task_Range) do
      Id := Given_Id;
      Put_Task(Id, "Recieved an ID.");
    end Handover_Id;
    delay Duration(Random(gen) mod 5);
    Put_Task(Id, "Dying...");
  end;

  Tasks : array (Task_Range) of Writer;
begin
  Rand_Int.Reset(gen);
  for I in Tasks'Range loop
    Tasks (I).Handover_Id(I);
  end loop;
  null;
end Main;
