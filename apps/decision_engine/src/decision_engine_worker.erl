%%%----------------------------------------------------------------------------
%%% @doc Master banker worker
%%% @author angelos
%%% @end
%%%----------------------------------------------------------------------------

-module(decision_engine_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("records/campaign.hrl").
-include("records/currency.hrl").
-include("records/node_campaign_budget.hrl").

%%% -----------------------------------------------------------------------------
%%% Constant definitions
%%% -----------------------------------------------------------------------------

-define(BID, 10).

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([
  start_link/0,                  % - starts and links the process in one step
  stop/0,                        % - stops it
  decide/1                       % - deciding to bid or not and for which campaign.
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

decide(BidRequest) ->
  gen_server:call(?SERVER, {decide, BidRequest}).


%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function: init/0
%% Purpose: Not many things to do here.
%% Args:   -.
%% Returns: An empty State.
%%-----------------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%-----------------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Not many things to do here.
%% Args:   -.
%% Returns: An empty State.
%%-----------------------------------------------------------------------------
handle_cast(_Info, State) ->      
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: It decides whether to bid or not and which campaign will be served.
%% Args:
%%    {decide, BidRequest}: Contains the parsed bid request data which will
%%                          be used to make the decision.
%% Returns: A tuple indicating whether to bid or not. In case of bidding the
%%          tuple contains the bid response details along with the amount
%%          to bid with.
%%-----------------------------------------------------------------------------
handle_call({decide, BidRequest}, _From, _State) ->
  UserID = extract_user_id(BidRequest),
  case UserID of
    no_id ->
      {no_bid, #node_campaign_budget{}, 0 };
    _ ->
      case decision_engine_aerospike_manager:lookup_user_id(UserID) of
        unknown_user ->
          {no_bid, #node_campaign_budget{}, 0 };
        _ ->
          choose_campaign(BidRequest)
      end
    end.


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

choose_campaign(BidRequest) ->
  Campaigns = decision_engine_mnesia_manager:get_campaigns(node()),  
  case Campaigns of
    [] ->
      {no_bid, #node_campaign_budget{}, 0 };
    [_] ->
      BidFloor = extract_bid_floor(BidRequest),
      FilteredCampaigns = remove_insufficient_budget_campaigns(Campaigns, BidFloor),
      SelectedCampaign = get_highest_roi_campaign(FilteredCampaigns),
      {bid, SelectedCampaign, ?BID}
  end.

extract_user_id(BidRequest) ->
  {_, User} = maps:get(imp, BidRequest, -1),
  case User of
    -1 ->
      no_id;
    _ ->
      {_, UserID} = maps:get(id, User, no_id),
      UserID
  end.

extract_bid_floor(BidRequest) ->
  {_, Impression} = maps:find(user, BidRequest),
  {_, BidFloor} = maps:find(bidfloor, Impression),
  BidFloor.

%%-----------------------------------------------------------------------------
%% Function: remove_insufficient_budget_campaigns/2
%% Purpose: Removes all the campaigns that don't have sufficient budget for 
%%          the bid request. The given bid floor is compared with the campaign
%%          budget.
%% Args:
%%    Campaigns: A list with all the running campaigns given to the bidder
%%               by the master banker.
%%    BidFloor: The bid floor extracted from the bid request. 
%% Returns: A list of campaigns with sufficient budget for the bid.
%%-----------------------------------------------------------------------------
remove_insufficient_budget_campaigns(Campaigns, BidFloor) ->
  [Campaign || Campaign <- Campaigns, Campaign#node_campaign_budget.remaining_budget > BidFloor].


%%-----------------------------------------------------------------------------
%% Function: calculate_ctr/0
%% Purpose: Dummy version for now, return 1.5% CTR.
%% Args:   -.
%% Returns: A dummy CTR.
%%-----------------------------------------------------------------------------
calculate_ctr() ->
  1.5.

%%-----------------------------------------------------------------------------
%% Function: get_highest_roi_campaign/1
%% Purpose: Find which campaign has the best ROI.
%% Args:
%%    Campaigns: A list of campaigns to compare their ROI.
%% Returns: The campaign with the highest ROI.
%%-----------------------------------------------------------------------------
get_highest_roi_campaign(Campaigns) ->
  CompareBudgetPerAction = fun(C1, C2) -> 
    calculate_ctr() * C1#node_campaign_budget.budget_per_action < calculate_ctr() * C2#node_campaign_budget.budget_per_action
  end,
  hd(lists:sort(CompareBudgetPerAction, Campaigns)).
