%%%-------------------------------------------------------------------
%% @doc 
%% @end
%%%-------------------------------------------------------------------

-module(file_count_server).
-behavior(gen_server).

-export([start_link/0]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {}).

-define(SERVER, ?MODULE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local,?SERVER}, ?MODULE, [], []).


%connect(FromNodeName) ->
%	gen_server:call({global, ?MODULE}, {connect, FromNodeName}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	{ok, #state{}}.
%%--------------------------------------------------------------


handle_call(_Req, _From, State) ->
	{reply, ok, State}.
%%--------------------------------------------------------------


handle_cast(_Req, State) ->
	{noreply, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%