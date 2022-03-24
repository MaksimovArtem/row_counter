%%%-------------------------------------------------------------------
%% @doc row_counter public API
%% @end
%%%-------------------------------------------------------------------

-module(row_counter_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    row_counter_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
