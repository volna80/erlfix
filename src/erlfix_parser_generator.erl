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
	
	%% map: key - field name, value - int FIX tag
	NumberByField = tmp_parse_number_by_field(select("fix/fields", XmlContent)),
	io:format("NumberByField=~s~n", [NumberByField]),
	
	%% map: key - message name, value - list of fields
	FieldsByMessage = tmp_parse_fields_by_message(select("fix/messages", XmlContent)),
	io:format("FieldsByMessage=~s~n", [FieldsByMessage]),
	
	%%header
	case file:open("./generated/erlfix_messages.hrl", [write]) of
		{ok, IOF} ->
			io:format(IOF, "%% Author: Nikolay Volnov ~n", []),
			io:format(IOF, "%% It is generated code, don't edit it~n",[]),
			io:format(IOF, "~n~n", []),
			io:format(IOF, "~s~n", [tmp_header( XmlContent)]),	
			file:close(IOF);
		{error, Reason} ->
			io:format("could not open file due to ~p.~n", Reason)
	end,
	
	%%parser
	case file:open("./generated/erlfix_parser.erl", [write]) of
		{ok, IOF2} ->
			io:format(IOF2, "~s~n", [tmp_message_parser(FieldsByMessage, NumberByField)]), 
			file:close(IOF2);
		{error, Reason2} ->
			io:format("could not open file due to ~p.~n", Reason2)
	end.

%% ==================================================================================
%% Small function's
%% ==================================================================================

tmp_parse_fields_by_message(E = #xmlElement{name = 'message'} ) ->
    [Name] = xslapply(fun tmp_parse_fields_by_message/1, select("@name", E)),
    ListOfFields = tmp_parse_list_of_fields(E),
    [{string:to_lower(Name), ListOfFields}];
tmp_parse_fields_by_message(E) ->
	built_in_rules(fun tmp_parse_fields_by_message/1, E).

tmp_parse_number_by_field(E = #xmlElement{name = 'field'}) ->
	[Tag] = xslapply(fun tmp_parse_number_by_field/1, select("@number", E)),
    [Name] = xslapply(fun tmp_parse_number_by_field/1, select("@name", E)),
	[{string:to_lower(Name), Tag}];
tmp_parse_number_by_field(E)->
	built_in_rules(fun tmp_parse_number_by_field/1, E).

tmp_parse_list_of_fields(E = #xmlElement{name = 'field'}) ->
	[Name] = xslapply(fun tmp_parse_list_of_fields/1, select("@name", E)),
	[string:to_lower(Name)];
tmp_parse_list_of_fields(E) ->
	built_in_rules(fun tmp_parse_list_of_fields/1, E).

%% ==================================================================================
%% ==================================================================================

tmp_message_parser(FieldsByMessage, NumberByField) ->	
	 "%% @doc TODO " ++ io_lib:nl() ++
	 "-module(erlfix_parser)" ++ io_lib:nl() ++ io_lib:nl() ++
	 "-include_lib(\"erlfix_messages.hrl\")." ++ io_lib:nl() ++ io_lib:nl() ++
	 "-export([encode/1, decode/1])." ++ io_lib:nl() ++ io_lib:nl() ++
	 "-define(SOH,[1])." ++ io_lib:nl() ++ io_lib:nl() ++ io_lib:nl() ++
	 "encode(Fix) ->" ++ io_lib:nl() ++
	 "ok." ++ io_lib:nl() ++ io_lib:nl() ++ io_lib:nl() ++
     "decode(MSG) when is_binary(MSG) ->" ++ io_lib:nl() ++
	 "decode(binary_to_list(MSG));" ++ io_lib:nl() ++
     "decode(MSG) ->" ++ io_lib:nl() ++
     "Tokens = string:tokens(MSG, ?SOH)," ++ io_lib:nl() ++  
     "Fun = fun(I) ->" ++ io_lib:nl() ++
	 "    string:tokens(I, \"=\")" ++ io_lib:nl() ++
	 "end," ++ io_lib:nl() ++
	 "FieldList = lists:map(Fun, Tokens)," ++ io_lib:nl() ++ 
	 "[[\"8\", BeginString], [\"9\", BodyLength], [\"35\", MsgType] | FieldList2] = FieldList," ++ io_lib:nl() ++
  	 "case MsgType of" ++ io_lib:nl() ++
	     tmp_message_parser_case(FieldsByMessage, NumberByField, []) ++
	 %%TODO "    \"0\" -> decodeHeartbeat(#heartbeat{header=#header{beginString=BeginString, bodyLength=BodyLength, msgType = MsgType}, trailer=#trailer{}}, FieldList2);
	 "    false -> false %% wrong format" ++ io_lib:nl() ++
    "end. " ++ io_lib:nl()++ io_lib:nl()++ io_lib:nl()++ io_lib:nl()++ io_lib:nl()
	 %% parse methods	
     %%xslapply(fun tmp_field_parser/1, select("messages/message", E)) 
	 .


%% \"0\" -> decodeHeartbeat(#heartbeat{header=#header{beginString=BeginString, bodyLength=BodyLength, msgType = MsgType}, trailer=#trailer{}}, FieldList2);
tmp_message_parser_case([], _ , Result)->
    Result;
tmp_message_parser_case([{Fields, Message} | REST], NumberByFields, Result) ->
	MsgType = "0",
	Result2 = "\"" ++ MsgType ++ "\" -> decode" ++ Message ++ "(#" ++ Message ++  "{header=#header{beginString=BeginString, bodyLength=BodyLength, msgType = MsgType}, trailer=#trailer{}}, FieldList2)	;" ++ io_lib:nl() ++ Result,
	tmp_message_parser_case(REST, NumberByFields, Result2).	
	

%%  Example output:
%%
%%  decodeheartbeat(Msg, [["56", Value] | Rest]) ->
%%  	Header = Msg#heartbeat.header#header{targetCompId=Value},
%%  	decodeheartbeat(Msg#heartbeat{header=Header}, Rest);
%%  decodeheartbeat(Msg, [["10", Value] | Rest]) ->
%%	  Trailer = Msg#heartbeat.trailer#trailer{checkSum=Value},
%%	  decodeheartbeat(Msg#heartbeat{trailer=Trailer}, Rest);
%%  decodeheartbeat(Msg, [[_, Value] | Rest]) ->
%%    decodeheartbeat(Msg,Rest).



%tmp_field_parser(E = #xmlElement{name='message'})->
%	[NameOrigin] = xslapply(fun tmp_parser/1, select("@name", E)),
%	Name = string:to_lower(NameOrigin),
%	FieldList = select("field", E),
%	[tmp_field_parser2(Name, FieldList),
%	 "decode" ++ Name ++ "(Msg, [[_, Value] | Rest]) -->" ++ io_lib:nl(),
%	 "    decode" ++ Name ++ "(Msg, Rest)." ++ io_lib:nl() ++ io_lib:nl() ++ io_lib:nl()
%	 ];
%tmp_field_parser(E) ->
%	built_in_rules(fun tmp_field_parser/1, E).


%tmp_field_parser2(MsgName, []) ->
%	[];
%tmp_field_parser2(MsgName, [E = #xmlElement{name = 'field'} | Rest ]) ->
%	[FieldNameOrigin] = xslapply(fun tmp_field_parser/1, select("@name", E)),
%    Field = string:to_lower(FieldNameOrigin),
%	["decode" ++ MsgName ++ "(Msg, [[\"10\", Value] | Rest]) ->" ++ io_lib:nl(),
%     "    decode" ++ MsgName ++ "(Msg#" ++ MsgName ++ "{" ++ Field ++ "=Value}, Rest);" ++ io_lib:nl(),
%	 tmp_field_parser2(MsgName, Rest)].

%% ==================================================================================
%% generate record's file
%% ==================================================================================
    
tmp_header(E = #xmlElement{name='fix'}) ->
	[xslapply(fun tmp_header/1, select("header", E)),
     xslapply(fun tmp_header/1, select("trailer", E)),
	 xslapply(fun tmp_header/1, select("messages/message", E))];    
tmp_header(E = #xmlElement{name='header'}) ->
	[FIRST | REST] = xslapply(fun tmp_header/1, select("field", E)),
	["-record(header,{", string:substr(FIRST,3), REST, "}).", io_lib:nl()];
tmp_header(E = #xmlElement{name = 'trailer'}) ->
	[FIRST | REST] = xslapply(fun tmp_header/1, select("field", E)),
	["-record(trailer,{", string:substr(FIRST,3), REST, "}).", io_lib:nl()];	
tmp_header(E = #xmlElement{name='field'}) ->
	[Value] = xslapply(fun tmp_header/1, select("@name", E)),
	", " ++ string:to_lower(Value);
tmp_header(E = #xmlElement{name='message'}) ->
	[Name] = xslapply(fun tmp_header/1, select("@name", E)),
	"-record(" ++ string:to_lower(Name) ++ "{header, trailer" ++ xslapply(fun tmp_header/1, select("field", E)) ++ "})." ++ io_lib:nl();
tmp_header(E) ->
	built_in_rules(fun tmp_header/1, E).

%% ==================================================================================
%% ==================================================================================

