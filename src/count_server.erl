%%%-------------------------------------------------------------------
%% @doc row_counter start gen_server.
%% @end
%%%-------------------------------------------------------------------

-module(count_server).
-behavior(gen_server).

-export([start_link/0]).
-export([count_rows/1]).

-export([get_erl_files_list/1]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {files_status = #{} :: map(),
				common_state = idle :: idle|running|err_access|err_no_entry|success}).

-define(SERVER, ?MODULE).
-define(SUPERVISOR, file_count_sup).
-define(SLASH_ASCII, 47).
-define(ERL_EXTENTION, ".erl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local,?SERVER}, ?MODULE, [], []).


count_rows(FolderPath) ->
	gen_server:cast(?SERVER, {count_rows, FolderPath}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	{ok, #state{common_state = idle}}.
%%--------------------------------------------------------------


handle_call(_Req, _From, State) ->
	{reply, ok, State}.


handle_cast({count_rows, FolderPath}, State) -> %= #state{common_state = idle}) ->
	NewState =
	case get_erl_files_list(FolderPath) of
		{error, DirError} -> State#state{common_state = calculate_state(DirError)};
		{FilesToCheck, Errors} ->
			ErrorStatusMap = maps:from_list(Errors),
			run_check(FilesToCheck),
			io:format("FilesToCheck ~p~n",[FilesToCheck]),
			State#state{files_status = ErrorStatusMap,
						common_state = running}
	end,
	io:format("State ~p~n",[NewState]),
	{noreply, NewState}.%;
%handle_cast({count_rows, _}, State = #state{common_state = ComState}) when ComState =/= idle ->
%	{noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_erl_files_list(RootFolderPath) ->
	case do_get_erl_files_list(RootFolderPath) of
		{[], Files, Errors} -> {Files, Errors};
		{RootSubDirs, Files, Errors} -> get_erl_files_list(RootSubDirs, {Files, Errors})
	end.

get_erl_files_list([], Acc) ->
	Acc;
get_erl_files_list([RootFolderPath|SubDirs], {Files, Errors}) ->
	case do_get_erl_files_list(RootFolderPath) of
		{[], RootFiles, RootErrors} ->
			get_erl_files_list(SubDirs, {lists:append(RootFiles,Files),
										 lists:append(RootErrors, Errors)});
		{RootSubDirs, RootFiles, RootErrors} ->
			get_erl_files_list(lists:append(RootSubDirs, SubDirs),
							   {lists:append(RootFiles,Files),
							    lists:append(RootErrors,Errors)})
	end.


do_get_erl_files_list(FolderPath) ->
	case file:list_dir(FolderPath) of
		{ok, FileNames} ->
			FullFileNames = get_full_paths(FolderPath, FileNames),
			GroupFiles =
			fun(File, Acc = {Dirs, Files, Errors}) ->
				case {get_file_type(File), get_file_extension(File)} of
					{{ok, regular}, erl} -> {Dirs, [File|Files], Errors};
					{{ok, directory}, _} -> {[File|Dirs], Files, Errors};
					{{error, Error},  erl} -> {Dirs, Files, [{File, Error}|Errors]};
					_Other -> Acc
				end
			end,
			lists:foldl(GroupFiles, {[],[],[]}, FullFileNames);
		{error, Error} -> {[],[],{FolderPath, Error}}
	end.
%%--------------------------------------------------------------


get_file_type(File) ->
	case file:read_file_info(File) of
		{ok, Data} ->
			case tuple_to_list(Data) of
				[file_info, _, regular|_] -> {ok, regular};
				[file_info, _, directory|_] -> {ok, directory}
			end;
		Error -> Error
	end.
%%--------------------------------------------------------------


get_full_paths(FolderPath, FileNames) ->
	FolderPathName =
	case lists:last(FolderPath) of
		?SLASH_ASCII -> FolderPath;
		_   -> FolderPath ++ "/"
	end,
	get_full_paths(FolderPathName, FileNames, []).

get_full_paths(_FolderPath, [], Acc) ->
 	Acc;
get_full_paths(FolderPath, [Name|T], Acc) ->
	get_full_paths(FolderPath, T, [FolderPath ++ Name|Acc]).
%%--------------------------------------------------------------


get_file_extension(FileName) ->
	case string:find(FileName, ?ERL_EXTENTION) of
		?ERL_EXTENTION -> erl;
		nomatch -> no_erl
	end.
%%--------------------------------------------------------------


calculate_state(eacces) -> err_access;
calculate_state(enoent) -> err_no_entry.
%%--------------------------------------------------------------


run_check(_FilesToCheck) ->
	ok.