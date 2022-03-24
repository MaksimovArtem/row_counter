%%%-------------------------------------------------------------------
%% @doc interface module for app
%% @end
%%%-------------------------------------------------------------------

-module(interface).

-export([count_rows/1, get_progress_report/0]).

-define(CATALOG_REGEXP, "^(.{1,2})|(/[^/. ]*)+/?$").

%-define(CATALOG_REGEX2P, "^.*\.erl$").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec count_rows(string()) -> ok.
count_rows(CatalogPath) when is_list(CatalogPath) ->
	{ok, Pattern} = re:compile(?CATALOG_REGEXP),
	case re:run(CatalogPath, Pattern) of
		{match,_} -> count_server:count_rows(CatalogPath);
		Other     -> count_rows(Other)
	end;

count_rows(_) ->
	io:format("Incorrect Input Data. Please use correct *NIX path: ~p~n",[?CATALOG_REGEXP]).
%%--------------------------------------------------------------


-spec get_progress_report() -> string().
get_progress_report() ->
	count_server:get_progress_report().