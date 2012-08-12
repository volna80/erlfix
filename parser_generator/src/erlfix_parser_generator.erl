%% Author: Nikolay Volnov
%% Created: 19.05.2012
%% Description: TODO: Add description to erlfix_parser_generator
-module(erlfix_parser_generator).

%%
%% Include files
%%

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("erlfix_macros.hrl").

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
	
	%% [{FieldName, TagNumber}]
	TagByName = tmp_parse_number_by_field(XmlContent),
	?DBG("~p ~n",[TagByName]),
	
	%% map: key - {name, msgtype} , value - [fieldName]
	FieldsByMessage = tmp_parse_fields_by_message(XmlContent),
	?DBG("~p ~n",[FieldsByMessage]),
	
	%% list: list of {name, msgtype}
	Messages = [K || {K,_} <- FieldsByMessage],
	
	%% [FieldName]
	HeaderFields = tmp_parse_header_fields(XmlContent),
	?DBG("~p ~n", [HeaderFields]),
	
	%% [FieldName]
    TrailerFields = tmp_parse_trailer_fields(XmlContent),
	?DBG("~p ~n", [TrailerFields]),
	
	%%header
	case file:open("../include/erlfix_messages.hrl", [write]) of
		{ok, IOF} ->
			io:format(IOF, "%% Author: Nikolay Volnov ~n", []),
			io:format(IOF, "%% It is generated code, don't edit it~n",[]),
			io:format(IOF, "~n~n", []),
			io:format(IOF, "~s~n", [tmp_record_header( XmlContent)]),	
			file:close(IOF);
		{error, Reason} ->
			io:format("could not open file due to ~p.~n", Reason)
	end,
	
	%%parser
	case file:open("../src/erlfix_parser.erl", [write]) of
		{ok, IOF2} ->
			io:format(IOF2, "~s~n", [tmp_message_parser_main_body(Messages)]),
			io:format(IOF2, "~s~n", [tmp_message_parser(FieldsByMessage, TagByName, HeaderFields, TrailerFields, [])]),
			file:close(IOF2);
		{error, Reason2} ->
			io:format("could not open file due to ~p.~n", Reason2)
	end.

%% ==================================================================================
%% ==================================================================================

tmp_parse_fields_by_message(E = #xmlElement{name = 'fix'})->
	xslapply(fun tmp_parse_fields_by_message/1, select("messages/message", E));
tmp_parse_fields_by_message(E = #xmlElement{name = 'message'} ) ->
    [Name] = xslapply(fun tmp_parse_fields_by_message/1, select("@name", E)),
	[MsgType] = xslapply(fun tmp_parse_fields_by_message/1, select("@msgtype", E)),
    ListOfFields = xslapply(fun tmp_parse_list_of_fields/1, select("field", E)),
    {{string:to_lower(Name), MsgType}, ListOfFields};
tmp_parse_fields_by_message(E) ->
	built_in_rules(fun tmp_parse_fields_by_message/1, E).

tmp_parse_number_by_field(E = #xmlElement{name='fix'}) ->
	xslapply(fun tmp_parse_number_by_field/1, select("fields/field", E));
tmp_parse_number_by_field(E = #xmlElement{name = 'field'}) ->
	[Tag] = xslapply(fun tmp_parse_number_by_field/1, select("@number", E)),
    [Name] = xslapply(fun tmp_parse_number_by_field/1, select("@name", E)),
	{string:to_lower(Name), Tag};
tmp_parse_number_by_field(E)->
	built_in_rules(fun tmp_parse_number_by_field/1, E).

tmp_parse_list_of_fields(E = #xmlElement{name = 'field'}) ->
	[Name] = xslapply(fun tmp_parse_list_of_fields/1, select("@name", E)),
	string:to_lower(Name);
tmp_parse_list_of_fields(E)->
	built_in_rules(fun tmp_parse_list_of_fields/1, E).

tmp_parse_header_fields(E = #xmlElement{name='fix'}) ->
	xslapply(fun tmp_parse_header_fields/1, select("header/field", E));
tmp_parse_header_fields(E = #xmlElement{name='field'}) ->
	[Name] = xslapply(fun tmp_parse_list_of_fields/1, select("@name", E)),
	string:to_lower(Name);
tmp_parse_header_fields(E) ->
	built_in_rules(fun tmp_parse_header_fields/1, E).

tmp_parse_trailer_fields(E = #xmlElement{name='fix'}) ->
	xslapply(fun tmp_parse_trailer_fields/1, select("trailer/field", E));
tmp_parse_trailer_fields(E = #xmlElement{name='field'}) ->
	[Name] = xslapply(fun tmp_parse_trailer_fields/1, select("@name", E)),
	string:to_lower(Name);
tmp_parse_trailer_fields(E) ->
	built_in_rules(fun tmp_parse_trailer_fields/1, E).


%% ==================================================================================
%% ==================================================================================

tmp_message_parser_main_body(Messages) ->	
	 "-module(erlfix_parser).\n\n" ++
	 "-include_lib(\"erlfix_messages.hrl\").\n\n" ++
     "-include_lib(\"erlfix_macros.hrl\").\n\n" ++
	 "-export([encode/1, decode/1]).\n\n" ++
	 "-define(SOH,[1]).\n\n" ++
	 "encode(Fix) ->\n" ++
	 "ok.\n\n" ++
     "decode(MSG) when is_binary(MSG) ->\n" ++
	 "    decode(binary_to_list(MSG));\n" ++
     "decode(MSG) ->\n" ++
     "    Tokens = string:tokens(MSG, ?SOH),\n" ++ 
     "    Fun = fun(I) ->  string:tokens(I, \"=\") end,\n" ++
	 "    FieldList = lists:map(Fun, Tokens),\n"  ++ 
	 "    [[\"8\", BeginString], [\"9\", BodyLength], [\"35\", MsgType] | FieldList2] = FieldList,\n" ++
  	 "    case MsgType of\n" ++
	     tmp_message_parser_case(Messages,[]) ++
	 %%"    \"0\" -> decodeHeartbeat(#heartbeat{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2);
	 "        false -> false %% wrong format\n" ++
     "    end. \n"
	 .


%% \"0\" -> decodeHeartbeat(#heartbeat{header=#header{beginString=BeginString, bodyLength=BodyLength, msgType = MsgType}, trailer=#trailer{}}, FieldList2);
tmp_message_parser_case([] , Result)->
    Result;
tmp_message_parser_case([{MsgName, MsgType} | REST], Result) ->
	Result2 = "        \"" ++ MsgType ++ "\" -> decode" ++ MsgName ++ "(#" ++ MsgName ++  "{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;" ++ io_lib:nl() ++ Result,
	tmp_message_parser_case(REST, Result2).	
	

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

tmp_message_parser([],_, _, _,Result) ->
	[Result];
tmp_message_parser([{{MsgName,_},Fields} | REST ], TagByName, HeaderFields, TrailerFields, Result) ->
	Result2 = Result ++ 
				  "decode" ++ MsgName ++ "(Msg,[]) -> \n" ++
                  "    Msg;\n" ++
				  tmp_message_parser_header(HeaderFields, MsgName, TagByName,[]) ++
				  tmp_message_parser_trailer(TrailerFields,MsgName, TagByName, []) ++
				  tmp_message_parser_body(Fields, MsgName, TagByName, []) ++
                 "decode" ++ MsgName ++ "(Msg, [[_, Value] | Rest]) ->\n" ++
                 "    decode" ++ MsgName ++ "(Msg,Rest).\n\n",
	tmp_message_parser(REST, TagByName,HeaderFields,TrailerFields, Result2).


tmp_message_parser_header([], MsgName, _, Result)->
	Result;
tmp_message_parser_header([FieldName | REST ],MsgName, TagByName, Result)->
	case  lists:keyfind(FieldName, 1, TagByName) of
		{_,FieldTag} ->
				Result2 = "decode" ++ MsgName ++ "(Msg, [[\"" ++ FieldTag ++ "\", Value] | Rest]) -> \n" ++
					      "    Header = Msg#" ++ MsgName ++ ".header#header{" ++ FieldName ++ "=Value},\n" ++
                          "    decode" ++ MsgName ++ "(Msg#" ++ MsgName ++ "{header=Header},Rest);\n" ++ Result,
	            tmp_message_parser_header(REST,MsgName,TagByName, Result2);
		false ->
			%% couldn't find a tag value, throw exception?
			tmp_message_parser_header(REST,MsgName,TagByName, Result)
	end.

tmp_message_parser_trailer([], MsgName, _, Result)->
	Result;
tmp_message_parser_trailer([FieldName | REST ],MsgName, TagByName, Result)->
	case  lists:keyfind(FieldName, 1, TagByName) of
		{_,FieldTag} ->
				Result2 = "decode" ++ MsgName ++ "(Msg, [[\"" ++ FieldTag ++ "\", Value] | Rest]) -> \n" ++
					      "    Trailer = Msg#" ++ MsgName ++ ".trailer#trailer{" ++ FieldName ++ "=Value},\n" ++
                          "    decode" ++ MsgName ++ "(Msg#" ++ MsgName ++ "{trailer=Trailer},Rest);\n" ++ Result,
	            tmp_message_parser_trailer(REST,MsgName,TagByName, Result2);
		false ->
			%% couldn't find a tag value, throw exception?
			tmp_message_parser_trailer(REST,MsgName,TagByName, Result)
	end.

tmp_message_parser_body([], MsgName, _, Result)->
	Result;
tmp_message_parser_body([FieldName | REST ],MsgName, TagByName, Result)->
	case  lists:keyfind(FieldName, 1, TagByName) of
		{_,FieldTag} ->
				Result2 = "decode" ++ MsgName ++ "(Msg, [[\"" ++ FieldTag ++ "\", Value] | Rest]) -> \n" ++
                          "    decode" ++ MsgName ++ "(Msg#" ++ MsgName ++ "{" ++ FieldName ++ "=Value}, Rest);\n" ++ Result,
	            tmp_message_parser_body(REST,MsgName,TagByName, Result2);
		false ->
			%% couldn't find a tag value, throw exception?
			tmp_message_parser_body(REST,MsgName,TagByName, Result)
	end.
	


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
    
tmp_record_header(E = #xmlElement{name='fix'}) ->
	[xslapply(fun tmp_record_header/1, select("header", E)),
     xslapply(fun tmp_record_header/1, select("trailer", E)),
	 xslapply(fun tmp_record_header/1, select("messages/message", E))];    
tmp_record_header(E = #xmlElement{name='header'}) ->
	[FIRST | REST] = xslapply(fun tmp_record_header/1, select("field", E)),
	["-record(header,{", string:substr(FIRST,3), REST, "}).\n\n"];
tmp_record_header(E = #xmlElement{name = 'trailer'}) ->
	[FIRST | REST] = xslapply(fun tmp_record_header/1, select("field", E)),
	["-record(trailer,{", string:substr(FIRST,3), REST, "}).\n\n"];	
tmp_record_header(E = #xmlElement{name='field'}) ->
	[Value] = xslapply(fun tmp_record_header/1, select("@name", E)),
	", " ++ string:to_lower(Value);
tmp_record_header(E = #xmlElement{name='message'}) ->
	[Name] = xslapply(fun tmp_record_header/1, select("@name", E)),
	"-record(" ++ string:to_lower(Name) ++ ",{header, trailer" ++ xslapply(fun tmp_record_header/1, select("field", E)) ++ "}).\n";
tmp_record_header(E) ->
	built_in_rules(fun tmp_record_header/1, E).

%% ==================================================================================
%% ==================================================================================

