-module(router).
 
-export([routes/0, port/0]).
 
routes() ->
  [
      {'_', [
      {"/ad/:userid", ad_request_handler, []},
      {"/openrtb2", openrtb2_bid_request_handler, []}
      ]}
    ].
