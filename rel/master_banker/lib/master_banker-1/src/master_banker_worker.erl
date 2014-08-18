%%%----------------------------------------------------------------------------
%%% @doc Master banker worker
%%% @author angelos
%%% @end
%%%----------------------------------------------------------------------------

-module(master_banker_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {bidders_count, bidders}).

-include("campaign.hrl").
-include("currency.hrl").
-include("node_campaign_budget.hrl").
-include("banker_campaign_budget.hrl").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([
  start_link/0,                  % - starts and links the process in one step
  stop/0,                        % - stops it
  bidder_announce/1,             % - used by the bidders to announce themselves 
                                 %   and allocate budget
  bidder_retire/1                % - used by the bidders before they die to move
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

bidder_announce(ID) ->
  gen_server:call(?SERVER, {bidder_announce, ID}).

bidder_retire(ID) ->
  gen_server:call(?SERVER, {bidder_retire, ID}).

%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

init([]) ->
  load_currencies_in_mnesia("root", "", "attalon_production"),
  CampaignRecords = mysql_manager:load_campaign_data("root", "", "attalon_production"),
  Campaigns = update_campaigns(CampaignRecords),
  CampaignBudgets = get_banker_campaign_budget(Campaigns),
  mnesia_manager:save_campaign_budgets(CampaignBudgets),
  crone:start([
  {
    {daily, {0, 1, am}},
    {io, fwrite, ["Goodmorning vietnammmm!!!!~n"]}
  }]),
  {ok, #state{bidders_count=0, bidders=[]}}.

handle_call({bidder_announce, ID}, _From, #state{ bidders_count=Count, bidders=Bidders }) ->
  % read the remaining daily budget from all nodes and for all campaigns
  % calculate the remaining daily budget for N+1 bidders
  % write budget per bidder in mnesia (and set fresh_budget=true)
  {reply, ok, #state{ bidders_count=Count+1, bidders=[ID] ++ Bidders }};

handle_call({bidder_retire, ID}, _From, #state{ bidders_count=Count, bidders=Bidders }) ->
  % read the remaining daily budget from all nodes and for all campaigns
  % calculate the remaining daily budget for N-1 bidders
  % write budget per bidder in mnesia (and set fresh_budget=true)
  {reply, ok, #state{ bidders_count=Count - 1, bidders = Bidders -- [ID] }}.

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

get_banker_campaign_budget(Campaigns) ->
  [#banker_campaign_budget{
      campaign_id = Campaign#campaign.id,
      remaining_days = get_campaign_remaining_days(Campaign),
      remaining_budget = calculate_remaining_budget(Campaign),
      daily_budget = calculate_daily_budget(Campaign)
    } || Campaign <- Campaigns].

calculate_remaining_budget(Campaign) ->
  Campaign#campaign.monetary_budget - calculate_daily_budget(Campaign).

calculate_daily_budget(Campaign) ->
  Duration = Campaign#campaign.duration,
  if Duration == 0 -> Campaign#campaign.monetary_budget; % Campaigns expiring today
    Duration < 0 -> 0; % Expired campaigns
    Duration > 0 -> Campaign#campaign.monetary_budget / Duration % Campaign to start campaign
  end.

get_campaign_remaining_days(Campaign) ->
  { { Y, M, D }, {_,_,_}} = calendar:universal_time(),
  { _, EndDate } =  Campaign#campaign.end_date,
  calendar:date_to_gregorian_days(EndDate) - calendar:date_to_gregorian_days(Y, M, D).

update_campaigns(Campaigns) ->
  [#campaign{
      id = Campaign#campaign.id, 
      monetary_budget = Campaign#campaign.monetary_budget,
      action_budget = Campaign#campaign.action_budget,
      currency_id = Campaign#campaign.currency_id,
      currency = mnesia_manager:find_currency_symbol(Campaign),
      start_date = Campaign#campaign.start_date,
      end_date = Campaign#campaign.end_date,
      duration = calculate_campaign_duration(Campaign)
    } || Campaign <- Campaigns].

calculate_campaign_duration(Campaign) ->
  { _, StartDate } =  Campaign#campaign.start_date,
  { _, EndDate } =  Campaign#campaign.end_date,
  calendar:date_to_gregorian_days(EndDate) - calendar:date_to_gregorian_days(StartDate).

load_currencies_in_mnesia(User, Password, Database) ->
  Currencies = mysql_manager:load_currency_data(User, Password, Database),
  [mnesia_manager:save_currency(Currency) || Currency <- Currencies].
