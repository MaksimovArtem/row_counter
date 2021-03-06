%%%-------------------------------------------------------------
%% @doc supervisor for every specific file that will be checked
%% @end
%%%-------------------------------------------------------------

-module(file_count_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SUP, ?MODULE).


%%%-------------------------------------------------------------
%% @doc This function starts supervisor that will supervise
%% file_X_count_server processes 
%% @end
%%%-------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).
%%%-------------------------------------------------------------

%% simple_one_for_one strategy is chosen since all file_X_count_server
%% processes are the same
init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [#{id       => file_count_checker, 
                    start    => {file_count_server, start_link, []},
                    restart  => transient,
                    type     => worker,
                    shutdown => 5000}],
    {ok, {SupFlags, ChildSpecs}}.

%% end of file_count_sup.erl