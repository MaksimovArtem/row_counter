%%%-------------------------------------------------------------------
%% @doc row_counter start gen_server.
%% @end
%%%-------------------------------------------------------------------

-module(count_server).
-behavior(gen_server).

-export([start_link/0]).
-export([count_rows/1, get_progress_report/0]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {files_status = #{} :: map(),
				common_state = idle :: idle|running|err_access|err_no_entry}).

-define(SERVER, ?MODULE).
-define(SUPERVISOR, file_count_sup).
-define(SLASH_ASCII, 47).
-define(ERL_EXTENTION, ".erl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local,?SERVER}, ?MODULE, [], []).


get_progress_report() ->
	gen_server:call(?SERVER, get_progress_report).


count_rows(FolderPath) ->
	gen_server:cast(?SERVER, {count_rows, FolderPath}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	{ok, #state{common_state = idle}}.
%%--------------------------------------------------------------

handle_call(get_progress_report, _From, State = #state{common_state = Status,
													   files_status = RootErrorMap}) when
		Status == err_access; Status == err_no_entry ->
		[RootFolder] = maps:keys(RootErrorMap),
	{reply, [{RootFolder,maps:get(RootFolder, RootErrorMap)}], State};
handle_call(get_progress_report, _From, State = #state{files_status = Status}) ->
	Data = get_current_progress(Status),
	{reply, Data, State}.


handle_cast({count_rows, FolderPath}, State) ->
	NewState =
	case get_erl_files_list(FolderPath) of
		{[], RootError = [{FolderPath, DirError}]} ->
			ErrorStatusMap = maps:from_list(RootError), 
			State#state{common_state = calculate_state(DirError),
						files_status = ErrorStatusMap};
		{FilesToCheck, Errors} ->
			ErrorStatusMap = maps:from_list(Errors),
			OngoingMap = run_check(FilesToCheck),
			State#state{files_status = maps:merge(ErrorStatusMap, OngoingMap),
						common_state = running}
	end,
	{noreply, NewState}.

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
		{error, Error} -> {[],[],[{FolderPath, Error}]}
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


run_check(FilesToCheck) ->
	run_check(FilesToCheck, []).

run_check([], FileServerNames) ->
	maps:from_list(FileServerNames);
run_check([File|OtherFilesToCheck], FileServerNames) ->
	ServerName = list_to_atom("file_" ++ File ++ "_count_server"),
	supervisor:terminate_child(file_count_sup, whereis(ServerName)),
	supervisor:start_child(file_count_sup,[File, ServerName]),
	run_check(OtherFilesToCheck, [{File, ServerName}|FileServerNames]).
%%--------------------------------------------------------------


get_current_progress(StatusMap) when is_map(StatusMap)->
	Iterator = maps:iterator(StatusMap),
	get_current_progress(maps:next(Iterator),[]).


get_current_progress(none, Acc) ->
	Acc;
get_current_progress({FileName, Error, Iterator},Acc) when Error == eacces;
														   Error == enoent;
														   Error == {no_translation, FileName} ->
	get_current_progress(maps:next(Iterator), [{FileName, Error}|Acc]);
get_current_progress({FileName, ServerName, Iterator}, Acc) when is_atom(ServerName) ->
	Data = file_count_server:get_progress(ServerName),
	get_current_progress(maps:next(Iterator), [{FileName, Data}|Acc]).

%% end of count_server.erl