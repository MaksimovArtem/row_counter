%%%-------------------------------------------------------------
%% @doc genserver that will count rows
%% @end
%%%-------------------------------------------------------------

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

%% This timeout is need to slow down program work to make it possible to check
%% progress report functionality. For test purposes only
%% With value equals 50, 20 rows per second will be handled
-define(TIMEOUT, 50).
%% Check that there are no lines there ONLY spaces/tabs are placed before 1st '%'
-define(INCORRECT_LINE_REGEXP, "^[\s]*(%)+.*$").
%% Check that we have 'smth % smth_else' in current line 
-define(CORRECT_LINE_REGEXP, "^.*(%)+.*$").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-------------------------------------------------------------
%% @doc This function starts file_FileName_count_server
%% This is called from count_server under file_count_sup 
%% @end
%%%-------------------------------------------------------------
start_link(FileName, ServerName) ->
    gen_server:start_link({local,ServerName}, ?MODULE, [FileName], []).


%%%-------------------------------------------------------------
%% @doc This function requests current progress for count_row
%% procedure 
%% @end
%%%-------------------------------------------------------------
get_progress(ServerName) ->
    gen_server:call(ServerName, get_progress).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Name]) ->
	%% File is opened during server start. File Descriptor is shared in state
	%% It will be closed when eof will be reached
    {ok, FD} = file:open(Name, [read]),
    %% count_row procedure will be start right after(Timeout=0) init func
    %% This is done since server should be available to get request for
    %% progress report. Such report can be stored in some DB but number of
    %% parallel servers can be too big so this[some custom table for every server]
    %% is not very effecient solution
    {ok, #state{name = Name, file_descriptor = FD, status = idle}, 0}.
%%%-------------------------------------------------------------


handle_call(get_progress, _From, State = #state{last_row_number = eof,
                                                count_of_code_lines = Count,
                                                status = success}) ->
    {reply, {success, eof, eof, Count}, State};
handle_call(get_progress, _From, State = #state{name = Name,
                                                last_row_number = LastChecked,
                                                count_of_code_lines = Count,
                                                status = Status}) ->
    %% Erlang has no built-in function that can say how many rows in file
    %% It is needed to provide progress report: handled X from Y rows
    %% The only way is to read full text before the procedure and count rows
    %% one by one. This is also not efficient solution: we have at elast one problem:
    %% - text will be red twice
    %% - text need to be stored somewhere to read it only once. Same problem as in liens 55-57
    CmdResult = os:cmd("cat " ++ Name ++ " | wc -l"),
    AllLinesCount = list_to_integer(string:trim(CmdResult, trailing, "\n")) + 1,
    {reply, {Status, LastChecked, AllLinesCount, Count}, State, ?TIMEOUT}.
%%%-------------------------------------------------------------


handle_cast(_Req, State) ->
    {noreply, State}.
%%%-------------------------------------------------------------


handle_info(timeout, State = #state{file_descriptor = FD,
                                    last_row_number = LastChecked,
                                    count_of_code_lines = OldCount}) ->
    case file:read_line(FD) of
        {ok, NewLine} ->
            {ok, PatternIncorrect} = re:compile(?INCORRECT_LINE_REGEXP),
            {ok, PatternCorrect} = re:compile(?CORRECT_LINE_REGEXP),
            NewCount =
            case is_code_row(NewLine, PatternIncorrect, PatternCorrect) of
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

is_code_row(Line, PatternIncorrect, PatternCorrect) ->
    re:run(Line, PatternIncorrect) == nomatch andalso
    re:run(Line, PatternCorrect) =/= nomatch.
%%%-------------------------------------------------------------


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