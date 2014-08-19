-module(router).
 
-export([routes/0]).
 
routes() ->
  [
    {'_', [
      {"/openrtb2", openrtb2_bid_request_handler, []}
    ]}
  ].
