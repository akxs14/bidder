%%-----------------------------------------------------------------------------
%% Description: It contains the campaign for a specific bidder node.
%% Fields: 
%%    node_id: The node's ID.
%%    campaign_id: The campaign's ID.
%%    remaining_budget: The remaining budget for the current day.
%%    next_day_budget: The budget for the following day.
%%-----------------------------------------------------------------------------
-record(node_campaign_budget, {
  node_id = "",
  campaign_id = 0,
  remaining_budget = 0,
  next_day_budget = 0,
  budget_per_action = 0,
  current_slot_budget = 0
  }).
