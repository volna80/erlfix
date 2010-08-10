%% Author: Nikolay Volnov
%% Created: 06.08.2010
-module(erlfix_parser_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlfix_messages.hrl").
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

simple_list_test() ->
    ?assertEqual([{beginString, "FIX.4.2"}], (erlfix_parser:parse_to_list("8=FIX.4.2"))).

login_list_test() ->
    ?assertEqual([{beginString, "FIX.4.2"}, {bodyLength, "65"}, {msgType, logon}, {msgSeqNum, 1}, {senderCompId, "BANZAI"},
		  {sendingTime, "20100806-16:16:57.011"}, {targetCompId, "EXEC"}, {encryptMethod, "0"}, {heartBtInt, 30}, {checkSum, "214"}],
		 (erlfix_parser:parse_to_list("8=FIX.4.29=6535=A34=149=BANZAI52=20100806-16:16:57.01156=EXEC98=0108=3010=214"))).

heartbeat_list_test() ->
    ?assertEqual([{beginString, "FIX.4.2"}, {bodyLength, "53"}, {msgType, heartbeat}, {msgSeqNum, 2}, {senderCompId, "BANZAI"},
		  {sendingTime, "20100808-09:10:43.110"}, {targetCompId, "EXEC"}, {checkSum, "163"}],
		 (erlfix_parser:parse_to_list("8=FIX.4.29=5335=034=249=BANZAI52=20100808-09:10:43.11056=EXEC10=163"))).


heartbeat_test() ->
    ?assertEqual((#heartbeat{header=#header{beginString="FIX.4.2", bodyLength="53", msgType=heartbeat, msgSeqNum=2, senderCompId="BANZAI",
					    sendingTime="20100808-09:10:43.110", targetCompId="EXEC"}, trailer=#trailer{checkSum="163"}}),
		 (erlfix_parser:parse("8=FIX.4.29=5335=034=249=BANZAI52=20100808-09:10:43.11056=EXEC10=163"))).

logon_test() ->
	?assertEqual( #logon{
						 header = #header{beginString = "FIX.4.2", bodyLength = "65", msgType = logon , msgSeqNum = 1, senderCompId = "BANZAI",
										  sendingTime = "20100806-16:16:57.011", targetCompId = "EXEC"},
						 heartBtInt = 30,
						 trailer = #trailer{checkSum = "214"}
						},
				  erlfix_parser:parse("8=FIX.4.29=6535=A34=149=BANZAI52=20100806-16:16:57.01156=EXEC98=0108=3010=214")).