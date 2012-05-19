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
				  string:tokens(I, "=")
		  end,
	FieldList = lists:map(Fun, Tokens), 
	[["8", BeginString], ["9", BodyLength], ["35", MsgType] | FieldList2] = FieldList,

	case MsgType of
		"0" -> decodeHeartbeat(#heartbeat{header=#header{beginString=BeginString, bodyLength=BodyLength, msgType = MsgType}, trailer=#trailer{}}, FieldList2);
		false -> false %% wrong format
    end.


%%
%% Local Functions
%%

decodeHeartbeat(Msg, []) ->
	Msg;
decodeHeartbeat(Msg, [["8", Value] | Rest]) ->
	Header = Msg#heartbeat.header#header{beginString=Value},
	decodeHeartbeat(Msg#heartbeat{header=Header},Rest);
decodeHeartbeat(Msg, [["9", Value] | Rest]) ->
	Header = Msg#heartbeat.header#header{bodyLength=Value},
	decodeHeartbeat(Msg#heartbeat{header=Header},Rest);
decodeHeartbeat(Msg, [["35", Value] | Rest]) ->
	Header = Msg#heartbeat.header#header{msgType=Value},
	decodeHeartbeat(Msg#heartbeat{header=Header}, Rest);
decodeHeartbeat(Msg, [["34", Value] | Rest]) ->
	Header = Msg#heartbeat.header#header{msgSeqNum=Value},
	decodeHeartbeat(Msg#heartbeat{header=Header}, Rest);
decodeHeartbeat(Msg, [["49", Value] | Rest]) ->
	Header = Msg#heartbeat.header#header{senderCompId=Value},
	decodeHeartbeat(Msg#heartbeat{header=Header}, Rest);
decodeHeartbeat(Msg, [["52", Value] | Rest]) ->
	Header = Msg#heartbeat.header#header{sendingTime=Value},
	decodeHeartbeat(Msg#heartbeat{header=Header}, Rest);
decodeHeartbeat(Msg, [["56", Value] | Rest]) ->
	Header = Msg#heartbeat.header#header{targetCompId=Value},
	decodeHeartbeat(Msg#heartbeat{header=Header}, Rest);
decodeHeartbeat(Msg, [["10", Value] | Rest]) ->
	Trailer = Msg#heartbeat.trailer#trailer{checkSum=Value},
	decodeHeartbeat(Msg#heartbeat{trailer=Trailer}, Rest);
decodeHeartbeat(Msg, [[_, Value] | Rest]) ->
    %%unknown field, ignore it
    decodeHeartbeat(Msg,Rest).
						


