%%%-------------------------------------------------------------------
%% @doc interface module for app
%% @end
%%%-------------------------------------------------------------------

-module(interface).

-export([count_rows/1, get_progress_report/0]).

-define(CATALOG_REGEXP, "^(.{1,2})|(/[^/. ]*)+/?$").

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
	ProgressReport = count_server:get_progress_report(),
	pretty_print(ProgressReport).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                    Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pretty_print([]) ->
	io:format("--------------------------------------------------------------~n");
pretty_print([{FileName, Error}|T]) when Error == eacces;
										 Error == enoent;
										 Error == {no_translation, FileName} ->
	io:format("--------------------------------------------------------------~n"),
	io:format("File: ~p~nStatus: ~p occured~nChecked: N/A Found: N/A~n",[FileName, Error]),
	pretty_print(T);
pretty_print([{FileName, {Status, eof, eof, Found}}|T]) ->
	io:format("--------------------------------------------------------------~n"),
	io:format("File: ~p~nStatus: ~p~nChecked: 100% Found: ~p~n",[FileName, Status, Found]),
	pretty_print(T);
pretty_print([{FileName, {Status, LastChecked, All, Found}}|T]) ->
	io:format("--------------------------------------------------------------~n"),
	Checked = LastChecked / All * 100, 
	io:format("File: ~p~nStatus: ~p~nChecked: ~.3f% Found: ~p~n",[FileName, Status, Checked, Found]),
	pretty_print(T).

%% end of interface.erl