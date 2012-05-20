%% Author: Nikolay Volnov
%% Created: 08.08.2010

-record(header,{beginString,bodyLength,msgType,msgSeqNum,senderCompId,sendingTime,targetCompId}).
-record(trailer,{checkSum}).

-record(heartbeat,{header,trailer}).
-record(logon,{header,encryptMethod,heartBtInt,trailer}).