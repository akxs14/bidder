-module(budget_pacer_mnesia_manager).


-include_lib("stdlib/include/qlc.hrl").

-include("records/campaign.hrl").
-include("records/currency.hrl").
-include("records/node_campaign_budget.hrl").

%%% -----------------------------------------------------------------------------
%% API Function Exports
%%% -----------------------------------------------------------------------------

-export([
  get_campaigns/1,
  save_campaigns/1
  ]).

%%% -----------------------------------------------------------------------------
%%% API Function Definitions
%%% -----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% API Calls for decision_engine_worker
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function: get_campaigns/1
%% Purpose: Returns a list with the running campaigns
%% Args: -
%% Returns: A list of running campaigns
%%-----------------------------------------------------------------------------
get_campaigns(NodeID) ->
  [Campaign || Campaign <- ets:tab2list(node_campaign_budget), Campaign#node_campaign_budget.node_id == NodeID].


%%-----------------------------------------------------------------------------
%% Function: save_campaigns/1
%% Purpose: Saves the give list of node_campaign_budget to mnesia
%% Args:
%%    Campaigns: The list of campaigns to persist.
%% Returns: -
%%-----------------------------------------------------------------------------
save_campaigns(NodeBudgetCampaigns) ->
  [ mnesia:activity(transaction, fun() ->
      mnesia:write(Campaign)
    end) || Campaign <- NodeBudgetCampaigns].

%%% -----------------------------------------------------------------------------
%%% Internal Function Definitions
%%% -----------------------------------------------------------------------------
