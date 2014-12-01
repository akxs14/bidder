-module(bidder_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
          {'_', [
                  {"/openrtb2",  openrtb2_bid_request_handler,  []},
                  {"/bidswitch", bidswitch_bid_request_handler, []}
          ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
          {env, [{dispatch, Dispatch}] }
  ]),
  bidder_sup:start_link().

stop(_State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
