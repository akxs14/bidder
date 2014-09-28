%%-----------------------------------------------------------------------------
%% Description: It contains the campaign currency as persisted in MySQL.
%% Fields: 
%%    id: The currency's ID.
%%    name: The currency's full name.
%%    symbol: The official currenct symbol (e.g. euro = EUR, pound = GBP).
%%    rate_to_euro: The latest exchange rate to euro.
%%-----------------------------------------------------------------------------
-record(currency, {
  id, 
  name, 
  symbol, 
  rate_to_euro
  }).
