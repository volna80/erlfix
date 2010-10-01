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
-export([logon_pref/0]).
 


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

sample1_test() ->
	Msg = erlfix_parser:parseFromFixToErl(<<56,61,70,73,88,46,52,46,50,1,57,61,54,53,1,51,53,61,65,1,51,52,61,57,1,52,57,61,66,65,78,90,65,73,1,53,50,61,50,48,49,48,
49,48,48,49,45,49,53,58,53,53,58,51,50,46,53,48,52,1,53,54,61,69,88,69,67,1,57,56,61,48,1,49,48,56,61,51,48,1,49,48,61,50,49,50,1>>)
	.


%% Performance tests
logon_pref() ->
	logon_pref(0).

logon_pref(100) ->
	ok;
logon_pref(Int) ->
	erlfix_parser:parseFromFixToErl("8=FIX.4.29=6535=A34=149=BANZAI52=20100806-16:16:57.01156=EXEC98=0108=3010=214"),
	logon_pref(Int + 1).