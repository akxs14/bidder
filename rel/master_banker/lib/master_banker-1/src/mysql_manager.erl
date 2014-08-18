-module(mysql_manager).

-include("campaign.hrl").
-include("currency.hrl").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([load_campaign_data/3, load_currency_data/3]).

%%-----------------------------------------------------------------------------
%% API Function Implementations
%%-----------------------------------------------------------------------------

load_campaign_data(User, Password, Database) ->
  connect_to_mysql(User, Password, Database),
  get_campaigns().

load_currency_data(User, Password, Database) ->
  connect_to_mysql(User, Password, Database),
  get_currencies().

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_currencies() ->
  Result = emysql:execute(hello_pool,
    <<"SELECT id, symbol, rate_to_euro FROM currencies">>),
  emysql_util:as_record(Result, currency, record_info(fields, currency)).

get_campaigns() ->
  Result = emysql:execute(hello_pool,
    <<"SELECT id, monetary_budget, action_budget, currency_id, start_date, end_date from campaigns">>),
  emysql_util:as_record(Result, campaign, record_info(fields, campaign)).

connect_to_mysql(User, Password, Database) ->
  application:start(emysql),
  emysql:add_pool(hello_pool, [
            {size,1},
            {user, User},
            {password, Password},
            {database, Database},
            {encoding,utf8}]).
