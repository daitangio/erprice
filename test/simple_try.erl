-module(simple_try).
%% -compile([{parse_transform, lager_transform},export_all]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
%% TEST CODE HERE


%% Must be first to  activate all the support servers
start_test()->
    %% manually start some application for unit testing
    %% I think it is wrong, but works...
    application:start(inets),    
    %% Run sasl for last...
    application:start(sasl),
    ok.

stop()->
    %%inets:stop(),
    ok.

stupid_test()->
    ?assertEqual(1,1).   

log_test() ->
    error_logger:info_msg("Simple SASL Info log ~p~n", [ yeppa ]).  

simple_url_request_test_disab()->    
    {ok, {{Version, 200, _ReasonPhrase}, Headers, _Body}} =
        httpc:request(get, {"https://gioorgi.com", []}, [], []),
    ?debugVal(Version),    
    ?debugVal(Headers),
    error_logger:info_msg("Simple url request").

map_learn_test() ->
    S= #{ count => 0},
    ?debugVal(S),
    #{ count:=PrevCount }=S,
    NewCount=PrevCount+1,
    %% Update existing value:
    S2=S#{ count := NewCount},
    ?debugVal(S2).

integration_fast_test()->
    {ok, GenServer}=gen_server:start_link(erprice_quote,[],[]),
    %% Trick: a negative value will generate a immediate notification:
    erprice_quote:dropPercentScan(GenServer,-0.05, 
                                  [  {"AAPL","NY"},
                                     {"ORCL","NY"}]),
    timer:sleep(2900).
      
%% simple_watch() ->
%%     {ok, GenServer}=gen_server:start_link(erprice_quote,[],[]),
%%     %% Force a price drop    
%%     ?debugVal(GenServer),
%%     %% Oracle call is fast...
%%     erprice_quote:watch(GenServer,"ORCL","NY",lessthen, 2000),
%%     erprice_quote:watch(GenServer,"SGR","MI",lessthen, 2000),
%%     timer:sleep(1500).

%% watch_test_() ->
%%     % 150 sec timeout
%%     {timeout, 150, fun () -> price_drop_percent_scan() end }.

%% price_drop_percent_scan()->
%%     {ok, GenServer}=gen_server:start_link(erprice_quote,[],[]),
%%     erprice_quote:dropPercentScan(GenServer,0.05, [ 
%%                                       {"ORCL","NY"},
%%                                       {"SGR","MI"},
%%                                       {"TRN","MI"},
%%                                       {"BMPS","MI"},
%%                                       {"ENEL","MI"}
%%                                     ]),
%%     timer:sleep(65000).
%% price_drop_scan() ->
%%     ?debugVal("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Scanner"),
%%     {ok, GenServer}=gen_server:start_link(erprice_quote,[],[]),
%%     erprice_quote:dropScan(GenServer, [ {"ORCL","NY",43},
%%                                         {"SGR","MI",7.5},
%%                                         {"TRN","MI",4.5},
%%                                         {"BMPS","MI",0.1}]),
    
%%     timer:sleep(65000).


%% lager_test()->    
%%     lager:info("Testing info"),
%%     ?assertEqual(1,1).



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

        

simple_api24_disab()->
    Quote=erprice_quote:get24price("SGR.MI"),
    ?assertEqual(true,is_float(Quote)),
    ?debugVal(Quote).

simple_api24_orcl_disab()->
    ?debugTime("Oracle from sole24 timing test",erprice_quote:get24price("ORCL.NY")),
    ok.


%% For yahoo api see http://www.jarloo.com/yahoo_finance/
%% "http://download.finance.yahoo.com/d/quotes.csv?s=ORCL&f=nab"

simple_yahooApi_disab()->
    ?debugTime("Yahoo access time:",
               erprice_quote:getYahooPrice("ORCL")),
    ok.


%%% Porfolio api
portfolio_buy1_test()->
    P=portfolio:new(100000),
    P1=portfolio:buy(P,"ORCL",1000,41.12),
    P2=portfolio:buy(P1,"ORCL",1000,41.12),
    error_logger:info_msg("Porfolio ~p~n", [ P2 ]),  
    P2.


-endif.


