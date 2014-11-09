-module(bidswitch_bid_response_builder).

-export([reply_no_bid/2, reply_bid/4]).

%% ============================================================================
%% Constant definitions
%% ============================================================================

-define(SEATID, "123456").


%% ============================================================================
%% API functions
%% ============================================================================

reply_no_bid(Id, Req) ->
  {ok, Req2} = cowboy_req:reply(200, 
    [{<<"content-type">>, <<"application/json">>}], 
    maps:to_json(prepare_no_bid_response(Id)),
    Req),
  Req2.

reply_bid(Req, BidRequest, SelectedCampaign, Bid) ->
  {ok, Req2} = cowboy_req:reply(200, 
    [{<<"content-type">>, <<"application/json">>}], 
    maps:to_json(prepare_bid_response(BidRequest, SelectedCampaign, Bid)),
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

prepare_bid_response(BidRequest, SelectedCampaign, Bid) ->
  ProductId = "XXXX",
  { Id, Currency } = extract_bid_req_data(BidRequest),
  { ImpId } = extract_bid_data(BidRequest),
  { UserId } = extract_user_data(BidRequest),
  #{
      cur => Currency,
      id => Id,
      seatbid => [#{
        seat => ?SEATID,
        bid => [#{
          id => generate_bid_id(),
          impid => ImpId,
          price => 10,
          nurl => assemble_ad_server_url(SelectedCampaign, UserId, ProductId),
          adid => SelectedCampaign,
          adomain => "www.attalon.com",
          cid => SelectedCampaign  
        }]
      }]
  }.

assemble_ad_server_url(SelectedCampaign, UserId, ProductId) ->
  lists:flatten(io_lib:format(
    "http://adsrv~p.attalon.com/~p/~p", [SelectedCampaign, UserId, ProductId])).

extract_bid_req_data(BidRequest) ->
  #{ id := Id, cur := Currency } = BidRequest,
  { Id, Currency }.


extract_bid_data(BidRequest) ->
  #{ impressions := Impressions } = BidRequest,
  [Impression | _] = Impressions,
  #{ id := ImpId } = Impression,
  { ImpId }.


generate_bid_id() ->
  {MegaSecs, Secs, MicroSecs} = erlang:now(),
  lists:flatten(io_lib:format("~p~p~p", [MegaSecs, Secs, MicroSecs])).


extract_user_data(BidRequest) ->
  #{ user := User } = BidRequest,
  #{ id := UserId } = User,
  { UserId }.


