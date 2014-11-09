%%-----------------------------------------------------------------------------
%% Description: It contains the campaign data as persisted in MySQL.
%% Fields: 
%%    id: The campaign's ID.
%%    monetary_budget: The monetary campaign budget in the currency specified
%%        by the user.
%%    action_budget: The overall budget in the user specified action, which
%%        may be impressions, clicks, downloads, purchases or else.
%%    currency_id: The id of the currency selected for the given campaign.
%%    start_date: The campaign's start date.
%%    end_date: The campaign's end date.
%%    duration: The campaign's duration in days.
%%-----------------------------------------------------------------------------
-record(campaign, {
  id,
  monetary_budget,
  action_budget,
  currency_id,
  currency,
  start_date,
  end_date,
  duration = 0,
  budget_per_action
  }).
