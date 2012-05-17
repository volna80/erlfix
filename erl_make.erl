%% Author: 1
%% Created: 17.05.2012
%% Description: TODO: Add description to erl_make
-module(erl_make).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([make/1]).

%%
%% API Functions
%%

make(Mode) ->
    case make:all([{d, Mode}]) of
    	error ->
    		error;
    	_ ->
    		erlfix_test_suite:test()
    end.