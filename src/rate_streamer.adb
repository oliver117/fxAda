with Ada.Text_IO;
with Ada.Exceptions;

with fxAda.Rates;

with Oanda_API;
with Oanda_API.Rates;

procedure Rate_Streamer is
   task type Quote_Printer;

   task body Quote_Printer is
      Quote : Oanda_API.Rates.Quote;
      Instrument : constant Oanda_API.Instrument_T := Oanda_API.To_Instrument("AUD_USD");
   begin
      loop
         fxAda.Rates.Wait;
         if fxAda.Rates.Has_Quote (Instrument) then
            Quote := fxAda.Rates.Quote (Instrument);

            Ada.Text_IO.Put_Line ("Bid: " & Oanda_API.Rate'Image (Quote.Bid));
         end if;
      end loop;
   end Quote_Printer;

   Printer_1 : Quote_Printer;

begin
   Ada.Text_IO.Put_Line ("Fetching Instrument List...");
   declare
      Instruments : constant Oanda_API.Instrument_Array :=
        Oanda_API.Rates.Get_Instruments (Oanda_API.Test_Account);
   begin
      Ada.Text_IO.Put_Line ("Starting fxAda...");
      fxAda.Rates.Start (Interval => 1.0,
                   Instruments => Instruments (Instruments'First + 6 .. Instruments'First + 6));
   end;

exception
   when Error : others =>
      Ada.Text_IO.Put_Line ("Exception, Message: " & Ada.Exceptions.Exception_Message (Error));
      abort Printer_1;


end Rate_Streamer;
