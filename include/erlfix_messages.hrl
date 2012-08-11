%% Author: Nikolay Volnov 
%% It is generated code, don't edit it


-record(header,{beginstring, bodylength, msgtype, sendercompid, targetcompid, onbehalfofcompid, delivertocompid, securedatalen, securedata, msgseqnum, sendersubid, senderlocationid, targetsubid, targetlocationid, onbehalfofsubid, onbehalfoflocationid, delivertosubid, delivertolocationid, possdupflag, possresend, sendingtime, origsendingtime, xmldatalen, xmldata, messageencoding, lastmsgseqnumprocessed, onbehalfofsendingtime}).

-record(trailer,{signaturelength, signature, checksum}).

-record(heartbeat,{header, trailer, testreqid}).
-record(newordersingle,{header, trailer, clordid, clientid, execbroker, account, settlmnttyp, futsettdate, handlinst, execinst, minqty, maxfloor, exdestination, processcode, symbol, symbolsfx, securityid, idsource, securitytype, maturitymonthyear, maturityday, putorcall, strikeprice, optattribute, contractmultiplier, couponrate, securityexchange, issuer, encodedissuerlen, encodedissuer, securitydesc, encodedsecuritydesclen, encodedsecuritydesc, prevclosepx, side, locatereqd, transacttime, orderqty, cashorderqty, ordtype, price, stoppx, currency, complianceid, solicitedflag, ioiid, quoteid, timeinforce, effectivetime, expiredate, expiretime, gtbookinginst, commission, commtype, rule80a, forexreq, settlcurrency, text, encodedtextlen, encodedtext, futsettdate2, orderqty2, openclose, coveredoruncovered, customerorfirm, maxshow, pegdifference, discretioninst, discretionoffset, clearingfirm, clearingaccount}).

