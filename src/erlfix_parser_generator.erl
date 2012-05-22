%% Author: Nikolay Volnov
%% Created: 19.05.2012
%% Description: TODO: Add description to erlfix_parser_generator
-module(erlfix_parser_generator).

%%
%% Include files
%%

-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xs, 
	[ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).

%%
%% Exported Functions
%%
-export([generate/1]).

%%
%% API Functions
%%

generate(XMLDoc) ->
	{XmlContent, _} = xmerl_scan:file(XMLDoc),
	
	case file:open("./generated/erlfix_messages.hrl", [write]) of
		{ok, IOF} ->
			io:format(IOF, "%% Author: Nikolay Volnov ~n", []),
			io:format(IOF, "%% It is generated code, don't edit it~n",[]),
			io:format(IOF, "~n~n", []),
			io:format(IOF, "~s~n", [template( XmlContent)]),	
			file:close(IOF);
		{error, Reason} ->
			io:format("could not open file due to ~p.~n", Reason)
	end.
    
template(E = #xmlElement{name='fix'}) ->
	[xslapply(fun template/1, select("header", E)),
     xslapply(fun template/1, select("trailer", E))];    
template(E = #xmlElement{name='header'}) ->
	[FIRST | REST] = xslapply(fun template/1, select("field", E)),
	["-record(header,{", string:substr(FIRST,3), REST, "}).", io_lib:nl()];
template(E = #xmlElement{name = 'trailer'}) ->
	[FIRST | REST] = xslapply(fun template/1, select("field", E)),
	["-record(trailer,{", string:substr(FIRST,3), REST, "}).", io_lib:nl()];	
template(E = #xmlElement{name='field'}) ->
	[Value] = xslapply(fun template/1, select("@name", E)),
	", " ++ string:to_lower(Value);
template(E) ->
	built_in_rules(fun template/1, E).

%%template_header(E = #xmlElement{name='field'}) ->
%%	[xmerl_xs:value_of(xmerl_xs:select(".",E)), ","];
%%template_header(E) ->
%%	built_in_rules(fun template_header/1, E).


%%
%% Local Functions
%%

