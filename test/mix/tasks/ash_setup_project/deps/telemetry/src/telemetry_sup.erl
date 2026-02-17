%% @private
-module(telemetry_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    TableHandler = #{id => telemetry_handler_table,
                     start => {telemetry_handler_table, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [telemetry_handler_table]},
    {ok, {SupFlags, [TableHandler]}}.
