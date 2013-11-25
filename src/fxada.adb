with Ada.Text_IO;

with Ada.Strings.Unbounded;

package body fxAda is

   use Ada.Strings.Unbounded;

   ---------------
   -- Has_Quote --
   ---------------

   function Has_Quote (Instr : in Instrument) return Boolean is
      (Quote_Storage.Has_Quote (Instr));


   -----------
   -- Quote --
   -----------

   function Quote (Instr : in Instrument) return Rates.Quote is
      (Quote_Storage.Quote (Instr));

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      Quote_Storage.Wait;
   end Wait;

   ----------
   -- Wait --
   ----------

   procedure Start (Interval : in Duration; Instruments : in Instrument_Array) is
   begin
      Ada.Text_IO.Put_Line ("Starting poller");
      Quote_Poller.Start (Interval, Instruments);
   end Start;

   ----------
   -- Wait --
   ----------

   procedure Stop is
   begin
      Quote_Poller.Stop;
   end Stop;

   -------------------
   -- Quote_Storage --
   -------------------

   protected body Quote_Storage_T is

      ---------------
      -- Has_Quote --
      ---------------

      function Has_Quote (Instr: in Instrument) return Boolean is
        (Rates.Quote_Maps.Contains (Quotes, Instr.Identifier));

      -----------
      -- Quote --
      -----------

      function Quote (Instr : in Instrument) return Rates.Quote is
      begin
         return Quotes.Element (Instr.Identifier);
      exception
         when Constraint_Error =>
            raise No_Quote with "There is no quote available for " & To_String (Instr.Display_Name) & '.';
      end Quote;

      ----------
      -- Wait --
      ----------

      entry Wait when Fresh is
      begin
         null;
      end;

      -----------
      -- Clear --
      -----------

      entry Clear when True is
      begin
         Quotes.Clear;
         if Wait'Count > 0 then
            Fresh := True;
            requeue Reset_Fresh;
         end if;
      end Clear;

      ------------
      -- Update --
      ------------

      entry Update (Instr : in Instrument; Q : in Rates.Quote) when True is
      begin
         Quotes.Include (Instr.Identifier, Q);
         Fresh := True;

         if Wait'Count > 0 then
            Fresh := True;
            requeue Reset_Fresh;
         end if;
      end Update;

      ------------
      -- Update --
      ------------

      entry Update (Q : in Rates.Quote_Array) when True is
      begin
         for I in Q'Range loop
            Quotes.Include (Q (I).Instrument, Q (I));
         end loop;

         if Wait'Count > 0 then
            Fresh := True;
            requeue Reset_Fresh;
         end if;
      end Update;

      -----------------
      -- Reset_Fresh --
      -----------------

      entry Reset_Fresh when Wait'Count = 0 and Fresh is
      begin
         Fresh := False;
      end;

   end Quote_Storage_T;

   ------------------------
   -- Quote_Polling_Task --
   ------------------------

   task body Quote_Polling_Task is

      Poll_Interval : Duration := 1.0;
      Polled_Instruments : Instrument_Array (0 .. 1);

      Started : Boolean := False;
   begin
      loop
         if not Started then
            select
               accept Start (Interval : in Duration; Instruments : in Instrument_Array) do
                  Ada.Text_IO.Put_Line ("Starting");
                  Poll_Interval := Interval;
                  Polled_Instruments := Instruments;
                  Started := True;
               end Start;
            or
               terminate;
            end select;
         else
            Ada.Text_IO.Put_Line ("Started");
            select
               accept Stop  do
                  Started := False;
               end Stop;
            or
               delay Poll_Interval;
               Quote_Storage.Update (Rates.Get_Quotes (Polled_Instruments));
            end select;
         end if;
      end loop;
   end Quote_Polling_Task;

end fxAda;
