with Oanda_API.Rates; use Oanda_API;

package fxAda.Rates is

   function Has_Quote (Instrument : in Instrument_T) return Boolean;
   function Quote (Instrument : in Instrument_T) return Oanda_API.Rates.Quote;
   procedure Wait;

   No_Quote : exception;

   procedure Start (Interval : in Duration; Instruments : in Instrument_Array);
   procedure Stop;

private
   protected type Quote_Storage_T is
      -- user operations
      function Has_Quote (Instrument : in Instrument_T) return Boolean;
      function Quote (Instrument : in Instrument_T) return Oanda_API.Rates.Quote;
      entry Wait;

      -- package operations
      entry Clear;
      entry Update (Instrument : in Instrument_T; Q : in Oanda_API.Rates.Quote);
      entry Update (Q : in Oanda_API.Rates.Quote_Array);
   private
      entry Reset_Fresh;

      Quotes : Oanda_API.Rates.Quote_Maps.Map := Oanda_API.Rates.Quote_Maps.Empty_Map;
      Fresh : Boolean := False;
   end Quote_Storage_T;

   Quote_Storage : Quote_Storage_T;

   task type Quote_Polling_Task is
      entry Start (Interval : in Duration; Instruments : in Instrument_Array);
      entry Stop;
   end Quote_Polling_Task;

   Quote_Poller : Quote_Polling_Task;

end fxAda.Rates;
