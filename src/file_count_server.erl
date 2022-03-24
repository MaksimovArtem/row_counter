%%%-------------------------------------------------------------------
%% @doc genserver that will count rows
%% @end
%%%-------------------------------------------------------------------

-module(file_count_server).
-behavior(gen_server).

-export([start_link/2, get_progress/1]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type line() :: integer() | eof.

-record(state, {name :: atom(), 
				file_descriptor,
				last_row_number = 0 :: line(),
				count_of_code_lines = 0 :: integer(),
				status = idle :: idle|running|success|internal_error}).

-define(TIMEOUT, 50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(FileName, ServerName) ->
	gen_server:start_link({local,ServerName}, ?MODULE, [FileName], []).


get_progress(ServerName) ->
	gen_server:call(ServerName, get_progress).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Name]) ->
	{ok, FD} = file:open(Name, [read]),
	{ok, #state{name = Name, file_descriptor = FD, status = idle}, 0}.
	
%%--------------------------------------------------------------


handle_call(get_progress, _From, State = #state{last_row_number = eof,
												count_of_code_lines = Count,
												status = success}) ->
	{reply, {success, eof, eof, Count}, State};
handle_call(get_progress, _From, State = #state{name = Name,
												last_row_number = LastChecked,
												count_of_code_lines = Count,
												status = Status}) ->
	CmdResult = os:cmd("cat " ++ Name ++ " | wc -l"),
	AllLinesCount = list_to_integer(string:trim(CmdResult, trailing, "\n")) + 1,
	{reply, {Status, LastChecked, AllLinesCount, Count}, State, ?TIMEOUT}.
%%--------------------------------------------------------------


handle_cast(_Req, State) ->
	{noreply, State}.
%%--------------------------------------------------------------


handle_info(timeout, State = #state{file_descriptor = FD,
									last_row_number = LastChecked,
									count_of_code_lines = OldCount}) ->
	case file:read_line(FD) of
		{ok, NewLine} ->
			NewCount =
			case is_code_row(NewLine) of
				true  -> OldCount + 1;
				false -> OldCount
			end,
			update_state(State, LastChecked + 1, NewCount);
		eof ->
			file:close(FD),
			update_state(State, LastChecked + 1);
		{error, Reason} -> update_state(State, Reason)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_code_row(_Line) -> true.
%%--------------------------------------------------------------


update_state(State, LastChecked) when is_integer(LastChecked)->
	{noreply, State#state{last_row_number = eof,
						  status = success}};
update_state(State, Reason)->
	io:format("Internal issue occcured: ~p~n",[Reason]),
	{noreply, State#state{status = internal_error}}.

update_state(State, LastChecked, NewCount) ->
	{noreply, State#state{last_row_number = LastChecked,
						  count_of_code_lines = NewCount,
						  status = running}, ?TIMEOUT}.

%%end of file_count_server.erl