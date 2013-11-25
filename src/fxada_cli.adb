--  Copyright (c) 2013 Oliver Kleinke
--
--  Permission is hereby granted, free of charge, to any person obtaining
--  a copy of this software and associated documentation files (the
--  "Software"), to deal in the Software without restriction, including
--  without limitation the rights to use, copy, modify, merge, publish,
--  distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to
--  the following conditions:
--
--  The above copyright notice and this permission notice shall be included
--  in all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
--  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
--  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;  use Ada.Characters;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Oanda_API;
with Oanda_API.Rates;

procedure fxAda_CLI is
   use Ada.Text_IO;
   use Oanda_API;

begin

   if Ada.Command_Line.Argument_Count = 0 then
      declare
         Instruments : constant Instrument_Array := Rates.Get_Instruments (Oanda_API.Test_Account);
      begin
         for I in Instruments'Range loop
            Put_Line
              ("Identifier: " &
               To_String (Instruments (I).Identifier) &
               Latin_1.HT &
               "Display Name: " &
               Ada.Strings.Unbounded.To_String (Instruments (I).Display_Name));
         end loop;
      end;
   elsif Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) /= "help"
   then
      declare
         Instr_Ident : constant Instrument_Identifier :=
           To_Identifier (Ada.Command_Line.Argument (1));
         Instr : constant Instrument := (Identifier => Instr_Ident, others => <>);
         Q : constant Rates.Quote := Rates.Get_Quote (Instr);
      begin
         Put_Line
           ("Instrument: " &
            To_String (Q.Instrument) &
            Latin_1.HT &
            "Bid: " &
            Rate'Image (Q.Bid) &
            Latin_1.HT &
            "Ask: " &
            Rate'Image (Q.Ask) &
            Latin_1.HT &
            "Time: " &
            Ada.Calendar.Formatting.Image (Q.Time) &
            Latin_1.HT &
            "Halted: " &
            Boolean'Image (Q.Halted));
      end;
   elsif Ada.Command_Line.Argument_Count = 3
     and then Ada.Command_Line.Argument (1) = "candles"
   then
      declare
         Sticks : constant Rates.Candlestick_Array :=
           Rates.Get_History
              (Instrument    =>
                  To_Identifier (Ada.Command_Line.Argument (3)),
               Granularity   =>
                  Rates.Granularity_T'Value
                    (Ada.Command_Line.Argument (2)),
               Count         => 10,
               Start_Time    => No_Time,
               End_Time      => No_Time,
               Candle_Format => Rates.Midpoint);
      begin
         for I in Sticks'Range loop
            Put_Line
              ("Open: " &
               Rate'Image (Sticks (I).Open_Mid) &
               Latin_1.HT &
               "High: " &
               Rate'Image (Sticks (I).High_Mid) &
               Latin_1.HT &
               "Low: " &
               Rate'Image (Sticks (I).Low_Mid) &
               Latin_1.HT &
               "Close: " &
               Rate'Image (Sticks (I).Close_Mid) &
               Latin_1.HT &
               " Time: " &
               Ada.Calendar.Formatting.Image (Sticks (I).Time));
         end loop;
      end;
   else
      Ada.Text_IO.Put_Line ("Usage:");
      Ada.Text_IO.Put_Line ("   fxada_cli [instrument]");
      Ada.Text_IO.Put_Line ("or");
      Ada.Text_IO.Put_Line ("   fxada_cli candles [timeframe instrument]");
   end if;
end fxAda_CLI;
