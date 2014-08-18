%%%----------------------------------------------------------------------------
%%% @doc Master banker worker
%%% @author angelos
%%% @end
%%%----------------------------------------------------------------------------

-module(decision_engine_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {}).

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([
  start_link/0,                  % - starts and links the process in one step
  stop/0,                        % - stops it
  decision_engine_announce/1,    % - used by the decision_engines to announce themselves 
                                 %   and allocate budget
  decision_engine_retire/1       % - used by the decision_engines before they die to move
                                 %   return their remaining budget back 
                                 %   (the budgets are read from mnesia)
  ]).

%% ---------------------------------------------------------------------------
%% gen_server Function Exports
%% ---------------------------------------------------------------------------

-export([                      % The behaviour callbacks
  init/1,                      % - initializes our process
  handle_call/3,               % - handles synchronous calls (with response)
  % handle_cast/2,             % - handles asynchronous calls  (no response)
  % handle_cast/3,
  handle_info/2,               % - handles out of band messages (sent with !)
  terminate/2,                 % - is called on shut-down
  code_change/3                % - called to handle code changes
  ]).

%% ---------------------------------------------------------------------------
%% API Function Definitions
%% ---------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

decision_engine_announce(ID) ->
  gen_server:call(?SERVER, {decision_engine_announce, ID}).

decision_engine_retire(ID) ->
  gen_server:call(?SERVER, {decision_engine_retire, ID}).

%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

init([]) ->
  {ok, #state{}}.

handle_call({decision_engine_announce, ID}, _From, State) ->
  {reply, ok, State};

handle_call({decision_engine_retire, ID}, _From, State) ->
  {reply, ok, State}.

handle_info(Info, State) ->      
    error_logger:info_msg("~p~n", [Info]), 
    {noreply, State}.          

terminate(_Reason, _State) ->  
    error_logger:info_msg("terminating~n"),
    ok.                        

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.               

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

