%%%----------------------------------------------------------------------------
%%% @doc Master banker worker
%%% @author angelos
%%% @end
%%%----------------------------------------------------------------------------

-module(budget_pacer_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("records/node_campaign_budget.hrl").

%%% -----------------------------------------------------------------------------
%%% Constant definitions
%%% -----------------------------------------------------------------------------

% Number of slots in a day. 
% For 2 minute slots, we have 30 x 24 = 720 slots
-define(DAILYSLOTS, 720).

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([
  start_link/0,                  % - starts and links the process in one step
  stop/0,                        % - stops it
  pace_budget/0
  ]).

%% ---------------------------------------------------------------------------
%% gen_server Function Exports
%% ---------------------------------------------------------------------------

-export([                      % The behaviour callbacks
  init/1,                      % - initializes our process
  handle_call/3,               % - handles synchronous calls (with response)
  handle_cast/2,               % - handles asynchronous calls  (no response)
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

pace_budget() ->
  gen_server:call(?SERVER, pace_budget).

%%-----------------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Not many things to do here.
%% Args:   -.
%% Returns: An empty State.
%%-----------------------------------------------------------------------------
handle_cast(_Info, State) ->      
  {reply, ok, State}.


handle_info(Info, State) ->
    error_logger:info_msg("~p~n", [Info]), 
    {noreply, State}.

terminate(_Reason, _State) ->  
    error_logger:info_msg("terminating~n"),
    ok.               

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.               


%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

init([]) ->
  crone:start([
  {
    {daily, {every, {120, sec} } },
    {budget_pacer_worker, pace_budget, []}
  }]),
  {ok, #state{}}.

handle_call(pace_budget, _From, State) ->
  Campaigns = budget_pacer_mnesia_manager:get_campaigns(node()),
  UpdatedCampaigns = [calculate_next_slot(Campaign) || Campaign <- Campaigns],
  budget_pacer_mnesia_manager:save_campaigns(UpdatedCampaigns),
  {reply, ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

calculate_next_slot(NodeCampaignBudget) ->
  RemainingBudget = NodeCampaignBudget#node_campaign_budget.current_slot_budget,
  NextSlotBudget = NodeCampaignBudget#node_campaign_budget.remaining_budget / ?DAILYSLOTS,
  #node_campaign_budget{ 
    node_id = NodeCampaignBudget#node_campaign_budget.node_id,
    campaign_id = NodeCampaignBudget#node_campaign_budget.campaign_id,
    remaining_budget = NodeCampaignBudget#node_campaign_budget.remaining_budget - NextSlotBudget,
    next_day_budget = NodeCampaignBudget#node_campaign_budget.next_day_budget,
    budget_per_action = NodeCampaignBudget#node_campaign_budget.budget_per_action,
    current_slot_budget = NextSlotBudget + RemainingBudget
  }.
