%% @doc The module is responsible for marshaling/unmarshaling FIX data to Erlan terms and back
%% @author: Nikolay Volnov
-module(erlfix_parser).

%%
%% Include files
%%

-include_lib("erlfix_messages.hrl").

%%
%% Exported Functions
%%
-export([parseFromFixToErl/1, parseFromErlToFix/1]).

-define(SOH,[1]).

%%
%% API Functions
%%

%% @doc parses an erlang object to a FIX message 

parseFromErlToFix(Msg) when is_tuple(Msg) ->
	parseFromErlToFix(tuple_to_list(Msg));
parseFromErlToFix([logon | Values]) ->
	decode(record_info(fields,logon), Values, []);
parseFromErlToFix([heartbeat | Values]) ->
	decode(record_info(fields,heartbeat), Values,[]).


%% @doc decode a list of values to FIX string message

-spec decode([atom()],[any()], string()) -> string().

decode([],[], Result) ->
	Result;
decode([header | Fields], [ Value | Other], Result) ->
  	Header = decodeHeader(
	record_info(fields,header), 
	lists:sublist(tuple_to_list(Value), 2, record_info(size, header)),
	[]),
    decode(Fields, Other, lists:append(Result, Header));
decode([trailer | Fields], [Value | Other], Result) ->
	Trailer = decodeTrailer( 
	  record_info(fields, trailer), 
	  lists:sublist(tuple_to_list(Value), 2, record_info(size, trailer)), 
	  []),
     decode(Fields, Other, lists:append(Result, Trailer));
decode([heartBtInt | Fields], [Value | Other], Result) ->
	decode(Fields, Other, lists:append([Result, "108=", integer_to_list(Value), ?SOH]));
decode([encryptMethod | Fields], [Value | Other], Result) ->
	decode(Fields, Other, lists:append([Result, "98=", Value, ?SOH]));
decode([_Unknown | Fields], [ Value | Other], Result) ->
	decode(Fields, Other, Result).

-spec decodeHeader([atom()], [any()], string()) -> string().

decodeHeader([],[], Result) ->
	Result;
decodeHeader([beginString | Fields], [Value | Other], Result) ->
	decodeHeader(Fields, Other, lists:append([Result, "8=", Value, ?SOH]));
decodeHeader([bodyLength | Fields], [Value | Other], Result) ->
	decodeHeader(Fields, Other, lists:append([Result, "9=", Value, ?SOH]));
decodeHeader([msgType | Fields], [ Value | Other ], Result ) ->
	decodeHeader( Fields, Other, lists:append( [ Result, "35=", msgTypeToStr(Value), ?SOH]));
decodeHeader([senderCompId | Fields], [ Value | Other ], Result ) ->
	decodeHeader( Fields, Other, lists:append( [ Result, "49=", Value, ?SOH]));
decodeHeader([targetCompId | Fields] , [ Value | Other], Result) ->
	decodeHeader( Fields, Other, lists:append( [ Result, "56=", Value, ?SOH]));
decodeHeader([msgSeqNum | Fields], [ Value | Other ], Result) ->
	decodeHeader( Fields, Other, lists:append( [ Result, "34=", integer_to_list(Value), ?SOH]));
decodeHeader([sendingTime | Fields], [ Value | Other ], Result) ->
	decodeHeader( Fields, Other, lists:append( [ Result, "52=", Value, ?SOH])).

-spec decodeTrailer([atom()],[any()], string()) -> string().

decodeTrailer([],[], Result) ->
	Result;
decodeTrailer([checkSum | Fields], [Value | Other], Result) ->
	decodeTrailer(Fields, Other, lists:append([Result, "10=", Value, ?SOH])).
  
%% @doc parses a string msg to one of erlfix records (see erlfix_messages).
parseFromFixToErl(MSG) ->
	ListOfFields = parse_to_list(MSG),

    case lists:keyfind(msgType, 1, ListOfFields) of
       {msgType, MsgType} ->  convert_to_record({MsgType,ListOfFields});
       false -> false %% @todo if false, throw exception
    end.

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
parseLogonMsg([{encryptMethod, VALUE} | Rest], Msg) ->
    parseLogonMsg(Rest, Msg#logon{encryptMethod = VALUE});
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
	Tokens = string:tokens(MSG, ?SOH),
	parse_field(Tokens).
 
parse_field([]) ->
	[];
parse_field([FIELD | REST]) ->
	[TAG,VALUE] = string:tokens(FIELD, "="),
	lists:append(
	  [convertTagToAtom(TAG,VALUE)],
	  parse_field(REST)
	).


%% help's functions

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

-spec msgTypeToStr(atom()) -> string().

msgTypeToStr(MsgType) ->
    case MsgType of
          logon -> "A";
          heartbeat -> "0";
          Other -> atom_to_list(Other)
   end.

parseMsgType(MsgType) ->
	case MsgType of
		"A" -> logon;
		"0" -> heartbeat;
		Other -> list_to_atom(Other)
	end.