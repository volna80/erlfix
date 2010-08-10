%% Module "parser" is responsible for marshaling/unmarshaling string FIX data to Erlan terms and back
%% Author: Nikolay Volnov
-module(erlfix_parser).

%%
%% Include files
%%

-include_lib("erlfix_messages.hrl").

%%
%% Exported Functions
%%
-export([parse_to_list/1, parse/1]).

-define(SOH,1).

%%
%% API Functions
%%

%% @doc the func parse a string msg to ne of erlfix records (see erlfix_messages).
parse(MSG) ->
	ListOfFields = parse_to_list(MSG),
    {msgType, MsgType} = lists:keyfind(msgType, 1, ListOfFields),
    %% todo if false, throw exception
    convert_to_record({MsgType,ListOfFields}).

convert_to_record({logon,ListOfFields}) ->
     parseLogonMsg(ListOfFields, 
				   #logon{header = parseHeader(ListOfFields, #header{}), trailer = parseTrailer(ListOfFields, #trailer{})}
				  );
convert_to_record({heartbeat,ListOfFields}) ->
	 #heartbeat{header = parseHeader(ListOfFields, #header{}), trailer = parseTrailer(ListOfFields, #trailer{})}.

parseLogonMsg([], Msg) ->
	Msg;
parseLogonMsg([{heartBtInt, VALUE} | REST ], Msg) ->
	parseLogonMsg(REST, Msg#logon{heartBtInt = VALUE});
parseLogonMsg([{UnknownTag, VALUE} | REST], Msg ) ->
	parseLogonMsg(REST, Msg).

parseHeader([],Header) ->
	Header;
parseHeader([{beginString, VALUE} | REST], Header) ->
	parseHeader(REST,Header#header{beginString = VALUE});
parseHeader([{bodyLength, VALUE} | REST], Header) ->
	parseHeader(REST, Header#header{bodyLength = VALUE});
parseHeader([{msgType, VALUE} | REST], Header) ->
	parseHeader(REST, Header#header{msgType = VALUE});
parseHeader([{senderCompId, VALUE} | REST], Header) ->
	parseHeader(REST, Header#header{senderCompId = VALUE});
parseHeader([{targetCompId, VALUE} | REST], Header) ->
	parseHeader(REST, Header#header{targetCompId = VALUE});
parseHeader([{msgSeqNum, VALUE} | REST], Header) ->
	parseHeader(REST, Header#header{msgSeqNum = VALUE});
parseHeader([{sendingTime, VALUE} | REST], Header) ->
	parseHeader(REST, Header#header{sendingTime = VALUE});
parseHeader([{UnknownTag, VALUE} | REST], Header) ->
	%%just ingore it
	parseHeader(REST , Header).

parseTrailer([], Trailer) ->
	Trailer;
parseTrailer([{checkSum, VALUE} | REST], Trailer) ->
	parseTrailer(REST, Trailer#trailer{checkSum = VALUE});
parseTrailer([{UnknownTag, VALUE} | REST], Trailer) ->
	parseTrailer(REST, Trailer).
	



%% @doc the func parses a string to the list of tuples. Example: [{beginString,"FIX.4.2"},{bodyLength,"53"},{msgType,heartbeat},{msgSeqNum,2}, ...]
parse_to_list(MSG) ->
	Tokens = string:tokens(MSG, [?SOH]),
	parse_field(Tokens).
 
parse_field([]) ->
	[];
parse_field([FIELD | REST]) ->
	[TAG,VALUE] = string:tokens(FIELD, "="),
	lists:append(
	  [convertTagToAtom(TAG,VALUE)],
	  parse_field(REST)
	).

convertTagToAtom(TAG, VALUE) ->
	case TAG of
		"8" -> {beginString, VALUE};
		"9" -> {bodyLength, VALUE};
		"10" -> {checkSum, VALUE};
		"34" -> {msgSeqNum, list_to_integer(VALUE)};
		"35" -> {msgType, parseMsgType(VALUE)};
		"49" -> {senderCompId, VALUE};
		"50" -> {senderSubId, VALUE};
		"52" -> {sendingTime, VALUE};
		"56" -> {targetCompId,VALUE};
		"98" -> {encryptMethod,VALUE};
		"108" -> {heartBtInt, list_to_integer(VALUE)};
		Other -> {list_to_atom(Other), VALUE}
	end.


parseMsgType(MsgType) ->
	case MsgType of
		"A" -> logon;
		"0" -> heartbeat;
		Other -> list_to_atom(Other)
	end.