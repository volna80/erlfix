%% Author: Nikolay Volnov
%% Created: 08.08.2010

-record(header,{beginString,bodyLength,msgType,senderCompId,targetCompId,msgSeqNum,sendingTime}).
-record(trailer,{checkSum}).

-record(logon,{header,heartBtInt,trailer}).
-record(heartbeat,{header,trailer}).
