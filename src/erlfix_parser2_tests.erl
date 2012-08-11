%% Author: Nikolay Volnov
%% Created: 18.05.2012
%% Description: TODO: Add description to erlfix_parser2_tests
-module(erlfix_parser2_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlfix_messages.hrl").
-include_lib("erlfix_macros.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%



%%
%% Local Functions
%%

from_fix_to_heartbeat_test() ->
	Msg = erlfix_parser:decode("8=FIX.4.29=5335=034=249=BANZAI52=20100808-09:10:43.11056=EXEC112=1234510=163"),
	?DBG("heartbeat = ~p ~n", [Msg]),
    ok.

