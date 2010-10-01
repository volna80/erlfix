%% Author: Nikolay Volnov
%% Created: Sep 29, 2010
%% The module is responsible for creating a socket, receiving and sending data from/to the socket
-module(erlfix_accepter).

%%
%% Include files
%%
-include_lib("erlfix_macros.hrl").

%%
%% Exported Functions
%%
-export([start/1, server/1]).

%%
%% API Functions
%%

start(Dict) ->
	?DBG("start() ~w",[Dict]),
	{ok,spawn_link(?MODULE,server, [Dict])}.

server(Dict) ->
	?DBG("server() ~w",[Dict]),
	{ok, Port} = dict:find(port, Dict),
	?DBG("listing the port[~w]",[Port]),
	{ok, LSock} = gen_tcp:listen(Port, [binary,{packet,0}, {active, false}]),
	wait_connection(LSock),
	ok.
%%
%% Local Functions
%%

wait_connection(ListenSocket)->
	?DBG("accepter:wait_connection() ~w", [ListenSocket]),
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	get_request(Socket).

get_request(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			?DBG("incoming: ~w", [Data]),
			Msg = erlfix_parser:parseFromFixToErl(Data),
		    erlfix_server:incoming_message(Socket, Msg),
			get_request(Socket);
		{error, closed} ->
			?DBG("socket[~w] is closed", [Socket]),
			  ok
      end.