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
 


from_fix_to_heartbeat_test() ->
    ?assertEqual((#heartbeat{header=#header{beginString="FIX.4.2", bodyLength="53", msgType=heartbeat, msgSeqNum=2, senderCompId="BANZAI",
					    sendingTime="20100808-09:10:43.110", targetCompId="EXEC"}, trailer=#trailer{checkSum="163"}}),
		 (erlfix_parser:parseFromFixToErl("8=FIX.4.29=5335=034=249=BANZAI52=20100808-09:10:43.11056=EXEC10=163"))).

from_heartbeat_to_fix_test() ->
	?assertEqual("8=FIX.4.29=5335=034=249=BANZAI52=20100808-09:10:43.11056=EXEC10=163",
				 erlfix_parser:parseFromErlToFix(
				   #heartbeat{header=#header{beginString="FIX.4.2", bodyLength="53", msgType=heartbeat, msgSeqNum=2, senderCompId="BANZAI",
					    sendingTime="20100808-09:10:43.110", targetCompId="EXEC"}, trailer=#trailer{checkSum="163"}})
				   ).

from_fix_to_logon_test() ->
	?assertEqual( #logon{
						 header = #header{beginString = "FIX.4.2", bodyLength = "65", msgType = logon , msgSeqNum = 1, senderCompId = "BANZAI",
										  sendingTime = "20100806-16:16:57.011", targetCompId = "EXEC"},
						 heartBtInt = 30, encryptMethod = "0",
						 trailer = #trailer{checkSum = "214"}
						},
				  erlfix_parser:parseFromFixToErl("8=FIX.4.29=6535=A34=149=BANZAI52=20100806-16:16:57.01156=EXEC98=0108=3010=214")).

from_logon_to_fix_test() ->
	Msg =  erlfix_parser:parseFromErlToFix(#logon{
						 header = #header{beginString = "FIX.4.2", bodyLength = "65", msgType = logon , msgSeqNum = 1, senderCompId = "BANZAI",
										  sendingTime = "20100806-16:16:57.011", targetCompId = "EXEC"},
						 heartBtInt = 30, encryptMethod = "0",
						 trailer = #trailer{checkSum = "214"}
						}),
    ?debugMsg( Msg ),
	?assertEqual(
	   "8=FIX.4.29=6535=A34=149=BANZAI52=20100806-16:16:57.01156=EXEC98=0108=3010=214",
	   Msg
	).
