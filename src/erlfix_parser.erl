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
decodeheartbeat(Msg, [["112", Value] | Rest]) -> 
    decodeheartbeat(Msg#heartbeat{testreqid=Value}, Rest);
decodeheartbeat(Msg, [[_, Value] | Rest]) ->
    decodeheartbeat(Msg,Rest).

decodenewordersingle(Msg,[]) -> 
 Msg;
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


