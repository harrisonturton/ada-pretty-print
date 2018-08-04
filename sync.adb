with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Pretty_Print;

procedure Sync is
   Start_Up_Time : constant Time     := Clock;
   No_Of_Tasks   : constant Positive := 2;
   subtype Task_Range is Positive range 1 .. No_Of_Tasks;

   package Task_Print is new Pretty_Print (No_Of_Tasks);
   use Task_Print;

   procedure Put_Line_Time (Line : String) is
   begin
      Put_Line ("At" & Duration'Image (Clock - Start_Up_Time) & "s: " & Line);
   end Put_Line_Time;

   procedure Put_Line_Id (Id : Task_Range; Line : String) is
   begin
      Put_Line_Time ("Task" & Task_Range'Image (Id) & " " & Line);
   end Put_Line_Id;

   task type Pinger is
      entry Give_Id (Given_Id : Task_Range);
      entry Kill;
   end Pinger;

   task body Pinger is
      Id : Task_Range;
   begin
      -- Wait for an ID
      Put_Line("Unknown task waiting for ID...");
      accept Give_Id (Given_Id : Task_Range) do
         Id := Given_Id;
         Put_Task (Id, "Recieved ID!");
      end Give_Id;
      Put_Line_Id (Id, "Finished recieving ID.");

      -- Wait for kill signal
      -- Does this block until we die?
      Put_Task(Id, "Waiting to die...");
      accept Kill do
        Put_Task(Id, "Recieved kill signal.");
      end Kill;
      Put_Task(Id, "About to die...");
   end Pinger;

   Tasks : array (Task_Range) of Pinger;
begin

   -- Sending IDs
   for i in Tasks'Range loop
      Put_Effect ((Bold, Underline), "Sending ID to Task" & Integer'Image (i) & "...");
      Tasks (i).Give_Id (i);
      Put_Line_Time ("Sent ID to Task" & Integer'Image (i));
   end loop;

   -- Sending kill signals
   Put_Line ("Waiting 3s until killing...");
   delay Duration (3);
   for i in Tasks'Range loop
      Put_Effect ((Bold, Underline), "Killing Task" & Integer'Image (i) & "...");
      Tasks (i).Kill;
      Put_Line_Time ("Sent kill signal to task" & Integer'Image (i));
   end loop;

   Put_Line ("Sent kill signals to every task. DONE!");
end Sync;
