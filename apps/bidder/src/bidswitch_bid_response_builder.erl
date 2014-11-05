-module(bidswitch_bid_response_builder).

-export([reply_no_bid/2, reply_bid/4]).

%% ============================================================================
%% Constant definitions
%% ============================================================================

-define(SEATID, "123456").
-define(BIDID, "123456").

%% ============================================================================
%% API functions
%% ============================================================================

reply_no_bid(Id, Req) ->
  {ok, Req2} = cowboy_req:reply(200, 
    [{<<"content-type">>, <<"application/json">>}], 
    maps:to_json(prepare_no_bid_response(Id)),
    Req),
  Req2.

reply_bid(Req, BidResponse, BidRequest, Bid) ->
  {ok, Req2} = cowboy_req:reply(200, 
    [{<<"content-type">>, <<"application/json">>}], 
    maps:to_json(extract_bid_req_data(BidRequest)),
     % jiffy:encode(<<"{\"rest\": \"Hello World!!!!\"}">>), 
    Req ),
  Req2.


%% ============================================================================
%% Internal functions
%% ============================================================================

prepare_no_bid_response(Id) ->
  #{
      id => Id,
      seatbid => []
  }.

prepare_bid_response(BidRequest) ->
  { Id, Currency } = extract_bid_req_data(BidRequest),
  #{
      cur => Currency,
      id => Id,
      seatbid => [{
        bid => [{
          id => "1"
        }]
        seat => ?SEATID
      }]
  }.

extract_bid_req_data(BidRequest) ->
  #{ id := Id, cur := Currency } = BidRequest,
  { Id, Currency }.


extract_bd_data(BidRequest) ->
  #{  } = BidRequest.

