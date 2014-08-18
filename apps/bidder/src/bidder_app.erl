-module(bidder_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Routes  = router:routes(),
  Dispatch = cowboy_router:compile(Routes),
  Port = port(),
  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}]}],
  {ok, _} = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
  bidder_sup:start_link().

stop(_State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
