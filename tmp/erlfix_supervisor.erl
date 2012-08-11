%%% -------------------------------------------------------------------
%%% Author  : Nikolay Volnov
%%% -------------------------------------------------------------------
-module(erlfix_supervisor).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include_lib("erlfix_macros.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1,
	 start/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


start(Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, [Args]).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(StartArgs) ->
	?DBG("init() ~w",[StartArgs]),
	
	AllEnv = application:get_all_env(),
	?DBG("env=~w", [AllEnv]),
	
	%%find type of an FIX engine (initiator or accepter)
	Dict = dict:from_list(AllEnv),
	{ok, Type} = dict:find(type, Dict),
	
	init(Type, Dict).
		

%% ====================================================================
%% Internal functions
%% ====================================================================

init(accepter, Args) ->
	
	%% one_for_one -  if one child process terminates and should be restarted, only that child process is affected.
	%% one_for_all -   if one child process terminates and should be restarted, all other child processes are terminated 
    %%                            and then all child processes are restarted.
	%% rest_for_one - if one child process terminates and should be restarted, the 'rest' of the child processes -- 
	%%                            i.e. the child processes after the terminated child process in the start order -- are terminated.
	%%                            Then the terminated child process and all child processes after it are restarted.
	%% simple_one_for_one - a simplified one_for_one supervisor, where all child processes are dynamically added instances of 
	%%                            the same process type, i.e. running the same code.

	
	%% child_spec() = {Id,StartFunc,Restart,Shutdown,Type,Modules}
	%% Restart = permanent | transient | temporary
    %% Shutdown = brutal_kill | int()>=0 | infinity
    %% Type = worker | supervisor

	ErlFixSocket = {'ErlFixSocket',{erlfix_accepter,start, [Args]}, permanent, 2000, worker, [erlfix_accepter]},
    ErlFixServer = {'ErlFixServer',{erlfix_server,start,[]}, permanent, 2000, worker,['erlfix_server']},	
    
	%%{ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
	%% If more than MaxR number of restarts occur in the last MaxT seconds, then the supervisor terminates all 
	%%    the child processes and then itself.
	{ok,{{one_for_one,100,100}, [ErlFixServer, ErlFixSocket]}};
init(initiator, Args) ->
	%%do nothing so far
	ok.
