-module(openrtb2_bid_request_handler).

%%% -----------------------------------------------------------------------------
%% API Function Exports
%%% -----------------------------------------------------------------------------

-export([ init/3,
          allowed_methods/2,
          content_types_accepted/2,
          handle_bid_request/2,
          terminate/3]).

%% ---------------------------------------------------------------------------
%% API Function Definitions
%% ---------------------------------------------------------------------------

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>,<<"json">>,[]}, reply_bid}
  ], Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%%% -----------------------------------------------------------------------------
%%% Internal Function Definitions
%%% -----------------------------------------------------------------------------

handle_bid_request(Req, State) ->
  {ok, Body, _Req1} = cowboy_req:body(Req),
  BidRequest = openrtb2_bid_request_parser:parse(Body),
  {ToBid, BidResponse, Bid} = decision_engine_worker:decide(BidRequest),
  case ToBid of
    no_bid ->
      HTTPResponse = reply_no_bid(Req, State);
    bid ->
      HTTPResponse = reply_bid(Req, State)
  end,
  {halt, HTTPResponse, State}.


reply_no_bid(Req, State) ->
  Body = <<"{\"rest\": \"Hello World!\"}">>,
  {Body, Req, State}.


reply_bid(Req, State) ->
  Body = <<"{\"rest\": \"Hello World!\"}">>,
  {Body, Req, State}.

