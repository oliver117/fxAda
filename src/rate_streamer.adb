with Ada.Text_IO;
with Ada.Exceptions;

with fxAda;

with Oanda_API;
with Oanda_API.Rates;

procedure Rate_Streamer is
   task type Quote_Printer;

   task body Quote_Printer is
      Quote : Oanda_API.Rates.Quote;
      Instr : constant Oanda_API.Instrument := (Identifier => Oanda_API.To_Identifier("AUD_USD"),
                                                others => <>);
   begin
      loop
         Ada.Text_IO.Put_Line ("Waiting... ");
         fxAda.Wait;
         if fxAda.Has_Quote (Instr) then
            Quote := fxAda.Quote (Instr);

            Ada.Text_IO.Put_Line ("Bid: " & Oanda_API.Rate'Image (Quote.Bid));
         end if;
      end loop;
   end Quote_Printer;

   Printer_1 : Quote_Printer;
   Printer_2 : Quote_Printer;

begin
   Ada.Text_IO.Put_Line ("Fetching Instrument List...");
   declare
      Instruments : constant Oanda_API.Instrument_Array :=
        Oanda_API.Rates.Get_Instruments (Oanda_API.Test_Account);
   begin
      Ada.Text_IO.Put_Line ("Starting fxAda...");
      fxAda.Start (Interval => 1.0,
                   Instruments => Instruments (Instruments'First .. Instruments'First + 1));
   end;

exception
   when Error : others =>
      Ada.Text_IO.Put_Line ("Exception, Message: " & Ada.Exceptions.Exception_Message (Error));
      abort Printer_1;
      abort Printer_2;


end Rate_Streamer;
