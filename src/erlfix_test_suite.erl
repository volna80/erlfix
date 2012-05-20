%% Author: 1
%% Created: 17.05.2012
%% Description: TODO: Add description to erlfix_test_suite
-module(erlfix_test_suite).

%%
%% Include files
%%
-include_lib("erlfix_macros.hrl").

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%

test()->
	%%eunit:test(erlfix_parser),
	eunit:test(erlfix_parser2),
	?DBG("HELLO ~w~n", erlfix_parser_generator:generate("./etc/FIX42.xml")),
	ok.


%%
%% Local Functions
%%

