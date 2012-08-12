-module(erlfix_parser).

-include_lib("erlfix_messages.hrl").

-include_lib("erlfix_macros.hrl").

-export([encode/1, decode/1]).

-define(SOH,[1]).

encode(Fix) ->
ok.

decode(MSG) when is_binary(MSG) ->
    decode(binary_to_list(MSG));
decode(MSG) ->
    Tokens = string:tokens(MSG, ?SOH),
    Fun = fun(I) ->  string:tokens(I, "=") end,
    FieldList = lists:map(Fun, Tokens),
    [["8", BeginString], ["9", BodyLength], ["35", MsgType] | FieldList2] = FieldList,
    case MsgType of
        "D" -> decodenewordersingle(#newordersingle{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "0" -> decodeheartbeat(#heartbeat{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        false -> false %% wrong format
    end. 

decodeheartbeat(Msg,[]) -> 
    Msg;
decodeheartbeat(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{onbehalfofsendingtime=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{lastmsgseqnumprocessed=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{messageencoding=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{xmldata=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{xmldatalen=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{origsendingtime=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{sendingtime=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{possresend=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{possdupflag=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{delivertolocationid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{delivertosubid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{onbehalfoflocationid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{onbehalfofsubid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{targetlocationid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{targetsubid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{senderlocationid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{sendersubid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{msgseqnum=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{securedata=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{securedatalen=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{delivertocompid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{onbehalfofcompid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{targetcompid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{sendercompid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{msgtype=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{bodylength=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{beginstring=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#heartbeat.trailer#trailer{checksum=Value},
    decodeheartbeat(Msg#heartbeat{trailer=Trailer},Rest);
decodeheartbeat(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#heartbeat.trailer#trailer{signature=Value},
    decodeheartbeat(Msg#heartbeat{trailer=Trailer},Rest);
decodeheartbeat(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#heartbeat.trailer#trailer{signaturelength=Value},
    decodeheartbeat(Msg#heartbeat{trailer=Trailer},Rest);
decodeheartbeat(Msg, [["112", Value] | Rest]) -> 
    decodeheartbeat(Msg#heartbeat{testreqid=Value}, Rest);
decodeheartbeat(Msg, [[_, Value] | Rest]) ->
    decodeheartbeat(Msg,Rest).

decodenewordersingle(Msg,[]) -> 
    Msg;
decodenewordersingle(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{onbehalfofsendingtime=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{lastmsgseqnumprocessed=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{messageencoding=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{xmldata=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{xmldatalen=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{origsendingtime=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{sendingtime=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{possresend=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{possdupflag=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{delivertolocationid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{delivertosubid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{onbehalfoflocationid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{onbehalfofsubid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{targetlocationid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{targetsubid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{senderlocationid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{sendersubid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{msgseqnum=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{securedata=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{securedatalen=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{delivertocompid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{onbehalfofcompid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{targetcompid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{sendercompid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{msgtype=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{bodylength=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{beginstring=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#newordersingle.trailer#trailer{checksum=Value},
    decodenewordersingle(Msg#newordersingle{trailer=Trailer},Rest);
decodenewordersingle(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#newordersingle.trailer#trailer{signature=Value},
    decodenewordersingle(Msg#newordersingle{trailer=Trailer},Rest);
decodenewordersingle(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#newordersingle.trailer#trailer{signaturelength=Value},
    decodenewordersingle(Msg#newordersingle{trailer=Trailer},Rest);
decodenewordersingle(Msg, [["440", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{clearingaccount=Value}, Rest);
decodenewordersingle(Msg, [["439", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{clearingfirm=Value}, Rest);
decodenewordersingle(Msg, [["389", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{discretionoffset=Value}, Rest);
decodenewordersingle(Msg, [["388", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{discretioninst=Value}, Rest);
decodenewordersingle(Msg, [["211", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{pegdifference=Value}, Rest);
decodenewordersingle(Msg, [["210", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{maxshow=Value}, Rest);
decodenewordersingle(Msg, [["204", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{customerorfirm=Value}, Rest);
decodenewordersingle(Msg, [["203", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{coveredoruncovered=Value}, Rest);
decodenewordersingle(Msg, [["77", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{openclose=Value}, Rest);
decodenewordersingle(Msg, [["192", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{orderqty2=Value}, Rest);
decodenewordersingle(Msg, [["193", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{futsettdate2=Value}, Rest);
decodenewordersingle(Msg, [["355", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedtext=Value}, Rest);
decodenewordersingle(Msg, [["354", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedtextlen=Value}, Rest);
decodenewordersingle(Msg, [["58", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{text=Value}, Rest);
decodenewordersingle(Msg, [["120", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{settlcurrency=Value}, Rest);
decodenewordersingle(Msg, [["121", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{forexreq=Value}, Rest);
decodenewordersingle(Msg, [["47", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{rule80a=Value}, Rest);
decodenewordersingle(Msg, [["13", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{commtype=Value}, Rest);
decodenewordersingle(Msg, [["12", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{commission=Value}, Rest);
decodenewordersingle(Msg, [["427", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{gtbookinginst=Value}, Rest);
decodenewordersingle(Msg, [["126", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{expiretime=Value}, Rest);
decodenewordersingle(Msg, [["432", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{expiredate=Value}, Rest);
decodenewordersingle(Msg, [["168", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{effectivetime=Value}, Rest);
decodenewordersingle(Msg, [["59", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{timeinforce=Value}, Rest);
decodenewordersingle(Msg, [["117", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{quoteid=Value}, Rest);
decodenewordersingle(Msg, [["23", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{ioiid=Value}, Rest);
decodenewordersingle(Msg, [["377", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{solicitedflag=Value}, Rest);
decodenewordersingle(Msg, [["376", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{complianceid=Value}, Rest);
decodenewordersingle(Msg, [["15", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{currency=Value}, Rest);
decodenewordersingle(Msg, [["99", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{stoppx=Value}, Rest);
decodenewordersingle(Msg, [["44", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{price=Value}, Rest);
decodenewordersingle(Msg, [["40", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{ordtype=Value}, Rest);
decodenewordersingle(Msg, [["152", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{cashorderqty=Value}, Rest);
decodenewordersingle(Msg, [["38", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{orderqty=Value}, Rest);
decodenewordersingle(Msg, [["60", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{transacttime=Value}, Rest);
decodenewordersingle(Msg, [["114", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{locatereqd=Value}, Rest);
decodenewordersingle(Msg, [["54", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{side=Value}, Rest);
decodenewordersingle(Msg, [["140", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{prevclosepx=Value}, Rest);
decodenewordersingle(Msg, [["351", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedsecuritydesc=Value}, Rest);
decodenewordersingle(Msg, [["350", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedsecuritydesclen=Value}, Rest);
decodenewordersingle(Msg, [["107", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{securitydesc=Value}, Rest);
decodenewordersingle(Msg, [["349", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedissuer=Value}, Rest);
decodenewordersingle(Msg, [["348", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedissuerlen=Value}, Rest);
decodenewordersingle(Msg, [["106", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{issuer=Value}, Rest);
decodenewordersingle(Msg, [["207", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{securityexchange=Value}, Rest);
decodenewordersingle(Msg, [["223", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{couponrate=Value}, Rest);
decodenewordersingle(Msg, [["231", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{contractmultiplier=Value}, Rest);
decodenewordersingle(Msg, [["206", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{optattribute=Value}, Rest);
decodenewordersingle(Msg, [["202", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{strikeprice=Value}, Rest);
decodenewordersingle(Msg, [["201", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{putorcall=Value}, Rest);
decodenewordersingle(Msg, [["205", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{maturityday=Value}, Rest);
decodenewordersingle(Msg, [["200", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{maturitymonthyear=Value}, Rest);
decodenewordersingle(Msg, [["167", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{securitytype=Value}, Rest);
decodenewordersingle(Msg, [["22", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{idsource=Value}, Rest);
decodenewordersingle(Msg, [["48", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{securityid=Value}, Rest);
decodenewordersingle(Msg, [["65", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{symbolsfx=Value}, Rest);
decodenewordersingle(Msg, [["55", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{symbol=Value}, Rest);
decodenewordersingle(Msg, [["81", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{processcode=Value}, Rest);
decodenewordersingle(Msg, [["100", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{exdestination=Value}, Rest);
decodenewordersingle(Msg, [["111", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{maxfloor=Value}, Rest);
decodenewordersingle(Msg, [["110", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{minqty=Value}, Rest);
decodenewordersingle(Msg, [["18", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{execinst=Value}, Rest);
decodenewordersingle(Msg, [["21", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{handlinst=Value}, Rest);
decodenewordersingle(Msg, [["64", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{futsettdate=Value}, Rest);
decodenewordersingle(Msg, [["63", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{settlmnttyp=Value}, Rest);
decodenewordersingle(Msg, [["1", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{account=Value}, Rest);
decodenewordersingle(Msg, [["76", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{execbroker=Value}, Rest);
decodenewordersingle(Msg, [["109", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{clientid=Value}, Rest);
decodenewordersingle(Msg, [["11", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{clordid=Value}, Rest);
decodenewordersingle(Msg, [[_, Value] | Rest]) ->
    decodenewordersingle(Msg,Rest).


