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
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Directory_Operations;
with Oanda_API.Rates;

procedure fxAda_CLI is
   Timeframe : Oanda_API.Rates.Granularity_T;
   Instrument : Oanda_API.Instrument_T;

   Timeframe_Set : Boolean := False;
   Information_Set : Boolean := False;
   Instrument_Set : Boolean := False;

   Exe_Name : constant String :=
     GNAT.Directory_Operations.Base_Name(Ada.Command_Line.Command_Name) & ": ";
begin
   -- parse command line
   declare
      use GNAT.Command_Line;
      use Ada.Characters;

      Config : Command_Line_Configuration;

      procedure Getopt_Callback (Switch, Param, Section: String) is
      begin
         if Switch = "-i" then
            Information_Set := True;
         elsif Switch = "-t" then
            begin
               Timeframe := Oanda_API.Rates.Granularity_T'Value(Param);
               Timeframe_Set := True;
            exception
               when Constraint_Error =>
                  Ada.Text_IO.Put_Line ("Invalid parameter: " & Param);
                  raise Exit_From_Command_Line;
            end;
         end if;
      end Getopt_Callback;

   begin
      Define_Switch (Config, "-i", Long_Switch => "--instrument-information",
                     Argument => "IDENTIFIER",
                     Help => "Displays information for the given instrument.");
      Define_Switch (Config, "-t:", Long_Switch => "--timeframe=",
                     Argument => "GRANULARITY",
                     Help => "Sets the Timeframe granularity. Possible values:" & Latin_1.LF &
                     Latin_1.HT & "S5, S10, S15, S30," & Latin_1.LF &
                     Latin_1.HT & "M1, M2, M3, M5, M10, M15, M30," & Latin_1.LF &
                       Latin_1.HT & "H1, H2, H3, H4, H6, H8, H12, D, W or M.");

      Set_Usage (Config,
                 Usage    => "[switches] instrument");

      Getopt (Config, Getopt_Callback'Unrestricted_Access); -- ugly, fix

      declare
         Arg : constant String := Get_Argument;
      begin
         if Arg /= "" then
            Instrument := Oanda_API.To_Instrument (Arg);
            Instrument_Set := True;
         end if;
         -- exception
      end;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         null;
      when GNAT.Command_Line.Invalid_Parameter =>
         Ada.Text_IO.Put_Line (Exe_Name & "invalid parameter for option '" & Full_Switch & "'");
   end;

   -- instrument specified? => instrument specific operations
   if Instrument_Set then
      if Information_Set then -- display information
         declare
            use Ada.Text_IO;
            use Oanda_API;

            I : constant Instrument_Information := Rates.Get_Instrument_Information (Test_Account, Instrument);
         begin
            Put_Line (Ada.Strings.Unbounded.To_String (I.Display_Name));
            Put_Line ("----------");
            Put_Line ("Identifier: " & To_String (I.Instrument));
            Put_Line ("Pip: " & Rate'Image (I.Pip));
            Put_Line ("Maximum trade size: " & Positive'Image (I.Max_Trade_Units));
            Put_Line ("Precision: " & Rate'Image (I.Precision));
            Put_Line ("Maximum trailing stop: " & Pips'Image (I.Max_Trailing_Stop));
            Put_Line ("Minimum trailing stop: " & Pips'Image (I.Min_Trailing_Stop));
            Put_Line ("Margin rate: " & Margin_Rate_T'Image (I.Margin_Rate));
         end;
      elsif Timeframe_Set then -- display historic quotes / candlestick data
         declare
            use Oanda_API;
            use Oanda_API.Rates;

            History : constant Candlestick_Array := Get_History (Instrument    => Instrument,
                                                                 Granularity   => Timeframe,
                                                                 Count         => 1,
                                                                 Candle_Format => Midpoint,
                                                                 Include_First => True);
            H : constant Candlestick := History (History'First);
         begin
            Ada.Text_IO.Put_Line ("Time: " & Ada.Calendar.Formatting.Image (H.Time));
            Ada.Text_IO.Put_Line ("Open: " & Rate'Image (H.Open_Mid));
            Ada.Text_IO.Put_Line ("High: " & Rate'Image (H.High_Mid));
            Ada.Text_IO.Put_Line ("Low: " & Rate'Image (H.Low_Mid));
            Ada.Text_IO.Put_Line ("Close: " & Rate'Image (H.Close_Mid));
         end;
      else -- get a simple quote
         declare
            use Oanda_API;
            use Oanda_API.Rates;

            Q : constant Quote := Get_Quote (Instrument);
         begin
            Ada.Text_IO.Put_Line ("Time: " & Ada.Calendar.Formatting.Image (Q.Time));
            Ada.Text_IO.Put_Line ("Bid: " & Rate'Image (Q.Bid));
            Ada.Text_IO.Put_Line ("Ask: " & Rate'Image (Q.Ask));
         end;
      end if;
   else -- no instrument specified => display list of instruments
      declare
         use Ada.Characters.Latin_1;
         use Oanda_API;
         use Oanda_API.Rates;
         Instruments : constant Instrument_Information_Array := Get_Instrument_Information (Oanda_API.Test_Account);
      begin
         for I in Instruments'Range loop
            Ada.Text_IO.Put_Line ("Name: " & Ada.Strings.Unbounded.To_String (Instruments (I).Display_Name)
                                  & ", ID: " & To_String (Instruments (I).Instrument));
         end loop;
      end;
   end if;
exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      null;
   when Oanda_API.No_Such_Instrument =>
      Ada.Text_IO.Put_Line ("No such instrument; please specify a valid instrument identifier.");
   when Error : Oanda_API.API_Error =>
      Ada.Text_IO.Put_Line ("API error: " &  Ada.Exceptions.Exception_Message (Error));
end fxAda_CLI;
