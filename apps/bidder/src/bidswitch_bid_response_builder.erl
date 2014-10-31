-module(bidswitch_bid_response_builder).

-export([reply_no_bid/2, reply_bid/3]).

%% ============================================================================
%% API functions
%% ============================================================================

reply_no_bid(Id, Req) ->
  {ok, Req2} = cowboy_req:reply(200, 
    [{<<"content-type">>, <<"application/json">>}], 
    maps:to_json(Id),
    Req),
  Req2.

reply_bid(Req, _BidResponse, Bid) ->
  {ok, Req2} = cowboy_req:reply(200, 
    [{<<"content-type">>, <<"application/json">>}], 
     jiffy:encode(<<"{\"rest\": \"Hello World!!!!\"}">>), Req ),
  Req2.


%% ============================================================================
%% Internal functions
%% ============================================================================

prepare_no_bid_response(Id) ->
  #{
      id => Id,
      seatbid => []
  }.
