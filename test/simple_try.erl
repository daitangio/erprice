-module(simple_try).
-compile([{parse_transform, lager_transform},export_all]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
%% TEST CODE HERE

-import(eredis, [create_multibulk/1]).

%% Must be first to  activate all the support servers
start_test()->
    lager:start(),
    inets:start(),
    ok.

stop()->
    inets:stop(),
    ok.

stupid_test()->
    ?assertEqual(1,1).   

lager_test()->    
    lager:info("Testing info"),
    ?assertEqual(1,1).

simple_url_request_disab()->    
    {ok, {{Version, 200, _ReasonPhrase}, Headers, Body}} =
        httpc:request(get, {"http://gioorgi.com", []}, [], []),
    lager:info("Version and Headers: ~p /// ~p ",[Version,Headers]),
    lager:info("Request Body:  ~p",[Body]).

%% Es di url di quotazione
%% http://www.milanofinanza.it/quotazioni/dettaglio/snam-2ae0363
%% http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!SRG.MI

simple_parse_test()->
    {ok,F}=file:read_file("./test_files/snam24.html"),
    [ _Orig |  Rest ]=re:replace(F,"databox.*td class=\"data.*\" style=\"color:#FFFFFF\">([0-9.]*)</td>.*","\\1"),
    [ Match | _ ] = Rest,
    %%lager:info("Quote for SNAM:~p",Match), 
    %% Ok, this route is stupid
    ?assertEqual([<<"5.22">>],Match),
    ?debugVal(Match),
    [ Az ] = Match,
    SnamQuote=list_to_float(bitstring_to_list(Az)),
    ?assertEqual(5.22,SnamQuote),
    ok.

extract24_it_test()->
    {ok,F}=file:read_file("./test_files/snam24.html"),
    SnamQuote =erprice_quote:extract24_quote(F),
    ?assertEqual(5.22,SnamQuote).

extract24_nsy_test()->
    {ok,F}=file:read_file("./test_files/oracle24.html"),
    Quote =erprice_quote:extract24_quote(F),
    ?assertEqual(41.07,Quote).

extract24_nsy2_test()->
    {ok,F}=file:read_file("./test_files/apple24.html"),
    Quote =erprice_quote:extract24_quote(F),
    ?assertEqual(98.59,Quote).



%% http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!AAPL.Q
%% ISIN DIRECT ACCESS:
%% 


%% Oracle parsing test
%% http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!ORCL.NY


simple_url_parse_disab()->
    {ok, {{_Version, 200, _ReasonPhrase}, Headers, Body}} =
        httpc:request(get, {"http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!SRG.MI", []}, [], []),
    lager:info("Headers: ~p",[Headers]),
    lager:info("Request Body:  ~p",[Body]),
    case re:run(Body,"databox.*class=\"data") of
        {match,Captured} -> lager:info("Got:  ~p",[Captured]);
        nomatch -> ?assertEqual(1,2)
    end,
    ok.
        

simple_api24_test()->
    Quote=erprice_quote:get24price("SGR.MI"),
    ?assertEqual(true,is_float(Quote)),
    ?debugVal(Quote).

simple_api24_orcl_test()->
    Quote=erprice_quote:get24price("ORCL.NY"),
    ?assertEqual(true,is_float(Quote)),
    ?debugVal(Quote).



-endif.


