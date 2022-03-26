%%%-------------------------------------------------------------------
%% @doc row_counter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(row_counter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SUP, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).


init([]) ->
    SupFlags = #{strategy => one_for_one},
    ChildSpecs = [#{id       => count_server,
                    start    => {count_server, start_link, []},
                    restart  => transient,
                    shutdown => 5000},
                  #{id       => file_count_sup,
                    start    => {file_count_sup, start_link, []},
                    restart  => transient,
                    type     => supervisor,
                    shutdown => 5000}],
    {ok, {SupFlags, ChildSpecs}}.

%% end of row_counter_sup.erl