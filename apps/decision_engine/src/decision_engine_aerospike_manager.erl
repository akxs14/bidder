-module(decision_engine_aerospike_manager).

-import(aerospike,
[connect/2, put/6, getAll/5, delete/4, shutdownAll/0 ]).

%%% -----------------------------------------------------------------------------
%%% Constant definitions
%%% -----------------------------------------------------------------------------

-define(DBHOST, "127.0.0.1").
-define(DBPORT, "3000").
-define(DBNS, "attalon").
-define(DBSET, "users").
-define(TIMEOUTMS, 30).

%%% -----------------------------------------------------------------------------
%% API Function Exports
%%% -----------------------------------------------------------------------------

-export([
  connect/0,
  lookup_user_id/2
  ]).

%%% -----------------------------------------------------------------------------
%%% API Function Definitions
%%% -----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% API Calls for decision_engine_worker
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function: connect/0
%% Purpose: Connects to the local Aerospike instance.
%% Args: -
%% Returns: A connection hook to the local Aerospike instance.
%%-----------------------------------------------------------------------------
connect() ->
  {ok, Connection} = aerospike:connect(?DBHOST, ?DBPORT),
  Connection.

%%-----------------------------------------------------------------------------
%% Function: lookup_user_id/1
%% Purpose: Looks if the user exists in the given list of users.
%% Args: -
%% Returns: A positive or negative for finding the user.
%%-----------------------------------------------------------------------------
lookup_user_id(Connection, UserID) ->
  Result = aerospike:getAll(Connection, ?DBNS, ?DBSET, UserID, ?TIMEOUTMS),
  case Result of
    [] ->
      unknown_user;
    [_] ->
      hd(Result)
  end.


%%% -----------------------------------------------------------------------------
%%% Internal Function Definitions
%%% -----------------------------------------------------------------------------
