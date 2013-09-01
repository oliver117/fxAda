fxAda
=====

A simple CLI program that uses OandaAPI and outputs a list of
instruments available for trading or a quote for a specific instrument.

Usage:
   fxada_cli [instrument]
or
   fxada_cli candles [timeframe instrument]

If fxada_cli is called without 'instrument' then a list of
available instruments is shown.


NOTE: OandaAPI accesses a sandbox server which does not return live rates.
