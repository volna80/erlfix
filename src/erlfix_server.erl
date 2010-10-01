%%% -------------------------------------------------------------------
%%% Author  : 1
%%% Description :
%%%
%%% Created : 25.08.2010
%%% -------------------------------------------------------------------
-module(erlfix_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include_lib("erlfix_macros.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/0, incoming_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->	
	?DBG("starting erlfix server",[]),
	gen_server:start_link({local, ?MODULE}, ?MODULE,[],[]).

incoming_message(Socket, Data) ->
	gen_server:cast(?MODULE, {incoming,Socket,Data}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	?DBG("erlfix_server:init()",[]),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
	?DBG("erlfix_server:handle_call() request=~w, from=~w, state=~w~n",[Request,From,State]),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
	?DBG("server:handle_cast() msg=~w, state=~w",[Msg,State]),
	case Msg of
		{incoming, Socket, FixMsg} ->
			?DBG("incoming message ~w",[FixMsg])
		end,
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	?DBG("handle_info() info=~w, state=~w~n",[Info,State]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	?DBG("erlfix_server:terminate() reason=~w, state=~w~n",[Reason,State]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	?DBG("erlfix_server:code_change() oldVsg=~w, state=~w, extra=~w~n",[OldVsn, State, Extra]),
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

