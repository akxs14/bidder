-module(mnesia_manager).


-include_lib("stdlib/include/qlc.hrl").

-include("campaign.hrl").
-include("currency.hrl").
-include("node_campaign_budget.hrl").
-include("banker_campaign_budget.hrl").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([
  init_db/0,
  save_currency/1,
  find_currency_symbol/1,
  save_campaign_budgets/1
  ]).

%%-----------------------------------------------------------------------------
%% API Function Definitions
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% API Calls for master_banker_app
%%-----------------------------------------------------------------------------

init_db() ->
  application:stop(mnesia),
  mnesia:create_schema(Nodes = [node()]),
  ok = mnesia:start(),
  create_campaign_budgets(Nodes),
  create_node_budgets(Nodes),
  create_currencies(Nodes).

%%-----------------------------------------------------------------------------
%% API Calls for master_banker_worker
%%-----------------------------------------------------------------------------

sum_node_budgets() ->
  ok.

create_node_campaign_budget(NodeID) ->
  ok.

remove_node_campaign_budget(NodeID) ->
  ok.

calculate_node_budgets() ->
  ok.

write_node_budgets() ->
  ok.

save_currency(Currency) ->
  mnesia:activity(transaction, fun() ->
    mnesia:write(Currency)
  end).

find_currency_symbol(Campaign) ->
  Results = mnesia:activity(transaction, fun() ->
    mnesia:read({currency, Campaign#campaign.currency_id})
  end),
  case hd(Results) of
    {currency, _, _, Symbol, _} ->
      Symbol;
    _ ->
      <<"Unknown">>
  end.

save_campaign_budgets(CampaignBudgets) ->
  [ mnesia:activity(transaction, fun() ->
      mnesia:write(CampaignBudget)
    end) || CampaignBudget <- CampaignBudgets].

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

create_campaign_budgets(Nodes) ->
  mnesia:create_table(banker_campaign_budget,
    [{ram_copies, Nodes},
    {attributes, record_info(fields, banker_campaign_budget)}]).

create_node_budgets(Nodes) ->
  mnesia:create_table(node_campaign_budget,
    [{ram_copies, Nodes},
    {attributes, record_info(fields, node_campaign_budget)}]).

create_currencies(Nodes) ->
  mnesia:create_table(currency,
    [{ram_copies, Nodes},
    {attributes, record_info(fields, currency)}]).
