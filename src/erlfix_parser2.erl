%% Author: 1
%% Created: 15.05.2012
%% Description: TODO: Add description to erlfix_parser2
-module(erlfix_parser2).

%%
%% Include files
%%

-include_lib("erlfix_messages.hrl").

%%
%% Exported Functions
%%
-export([encode/1, decode/1]).

-define(SOH,[1]).

%%
%% API Functions
%%

encode(Fix) ->
	ok.


decode(MSG) when is_binary(MSG) ->
	decode(binary_to_list(MSG));
decode(MSG) ->
    Tokens = string:tokens(MSG, ?SOH),  
    Fun = fun(I) ->
				  [Key,Value] = string:tokens(I, "="),
				  {Key,Value}
		  end,
	Fields = lists:map(Fun, Tokens), 
	ok.



%%
%% Local Functions
%%

