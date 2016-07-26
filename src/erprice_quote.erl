-module(erprice_quote).
%% Native ask for hipe? Win does not support it
%% -compile([ native,export_all]).
-compile([ export_all]).
-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, code_change/3,terminate/2,handle_info/2]).

%% For eunit:
-export([extract24_quote/1,get24price/1]).

%% @doc Gen server master watcher. 
%% TODO Custom error log http://erlang.org/documentation/doc-4.9.1/doc/design_principles/error_logging.html
%% 

%% @doc how much parallel connections for getting quote?
%% -define(MAX_CONNECTIONS,2).

%% @doc delay between two consecutive requests on the same quote resource
-define(COLD_DOWN_MS,70000).


%%% BASIC API
%% @doc Spawn a child which will notify the price drop to the Gen Server
watch(GenServer, Company, Market,lessthen,Quote)->
    NewPid=spawn(?MODULE,watchDrop,[Company,Market,Quote,GenServer]),
    NewPid.

%% i.e. dropPercentScan(Pid,0.1, [ {"BMPS","MI"}])
dropPercentScan(GenServer, Percent, [ Tick | Rest] ) ->
    { Company, Market } = Tick,
    %% Extract the quote
    CurrentQuote =  getQuote(Company,Market),
    DropLimit= CurrentQuote*(1-Percent),
    watch(GenServer,Company, Market,lessthen,DropLimit),
    dropPercentScan(GenServer,Percent,Rest);
dropPercentScan(GenServer,_Percent,[])->
    GenServer.



%% Scan a set of stuff for the requested drop
dropScan( GenServer, [Tick | Rest ] ) ->
    { Company, Market,Quote } = Tick,
    watch(GenServer,Company, Market,lessthen,Quote),
    dropScan(GenServer, Rest);
dropScan(GenServer,[]) ->
    GenServer.


%% @doc Spawned by watch
watchDrop(Company,Market,Quote,GenServer)->
    %% error_logger:info_msg("Monitoring lessthen ~p",Company),
    CurrentQuote =  getQuote(Company,Market),
    error_logger:info_msg("Comparing ~p ~p < ~p", [Company,CurrentQuote,Quote]),
    if CurrentQuote < Quote  -> 
            error_logger:info_msg("Trigger Comparing ~p ~p < ~p", [Company,CurrentQuote,Quote]),
            gen_server:cast(GenServer, {self(),drop,Company,CurrentQuote});           
       true -> %% error_logger:info_msg(" Retrying... "),
               %% Sleep and reschedule.
               %% To avoid detection, we add also some random time inside it.              
               timer:sleep( trunc(?COLD_DOWN_MS/2 + trunc(rand:uniform()*(?COLD_DOWN_MS/2))) ),               
               watchDrop(Company,Market,Quote,GenServer)
    end.


    

%%% GEN SERVER Implementation



start_link()->
    {ok, GenServer}=gen_server:start_link({local, ?MODULE }, ?MODULE, [], []),
    GenServer.

init(_WhateverYouLike) ->
    State=[],
    {ok,State}.

handle_call(quote,_From, State) ->
    {reply,use_cast, State}.


handle_cast({_WorkerPid, drop, Company,CurrentQuote},State)->
    error_logger:info_msg("NOTIFY Drop on ~p ~p",[Company,CurrentQuote]),
    %% TODO: implement some notification action
    %% Also if the trigger must be rescheduled provide additional actions
    {noreply,State}.

%% handle_info({'EXIT', _Pid, _Reason}, State) ->
%%     %% ..code to handle exits here..
%%     {noreply, State}.

handle_info(_UnknownMessage,State) ->
    {noreply, State}.

terminate(normal,_State)->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%% Low level web service quote services

getQuote(Company,Market)->
    CurrentQuote = 
        case Market of
            "NY"  -> getYahooPrice(Company) ;
            _Other -> get24price(string:concat(string:concat(Company,"."),Market))                        
        end,
    CurrentQuote.



%% Example: http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!ORCL.NY
%% The page is fat but the parsing is quite easy
%% The generic form is something like a nested databox / data varup like...
%% <div class="databox"><table border="0" cellpadding="0" cellspacing="0"><tbody><tr><td class="desc" style="width:60px;">ULTIMO<br>PREZZO</td><td class="data varup" style="color:#FFFFFF">5.22</td></tr></tbody></table></div>
extract24_quote(String)->
    [ _Orig |  Rest ]=re:replace(String,"databox.*td class=\"data.*\" style=\"color:#FFFFFF\">([0-9.]*)</td>.*","\\1"),
    [ Match | _ ] = Rest,    
    %% Ok, this route is stupid but works
    %% GG: Sometimes you can get a badmatch (for instance for a error)
%% Error in process <0.78.0> on node 'erprice@127.0.0.1' with exit value:
%% {{badmatch,33},
%%  [{erprice_quote,extract24_quote,1,
%%                  [{file,"c:/giorgi/code/erprice/src/erprice_quote.erl"},
%%                   {line,123}]},
%%   {erprice_quote,watchDrop,4,
%%                  [{file,"c:/giorgi/code/erprice/src/erprice_quote.erl"},
%%                   {line,54}]}]}

    [ Az ] = Match,
    Quote=list_to_float(bitstring_to_list(Az)),
    Quote.


%% Sole24 API
%% The API is based on a "ticker" parameters
%% http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!ENEL.MI
get24price(Ticker) ->
    %% Concat the ticker
    CallUrl =string:concat("http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!",Ticker),
    %%error_logger:info_msg("Calling ~p",[CallUrl]),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {CallUrl, []}, [], []),
    FloatQuote=extract24_quote(Body),
    FloatQuote.

getYahooPrice(Ticker) ->
    %% nab= name, ask, bid.
    CallUrl =string:concat("http://download.finance.yahoo.com/d/quotes.csv?f=a&s=",Ticker),
    %%error_logger:info_msg("Calling ~p",[CallUrl]),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {CallUrl, []}, [], []),
    { Quote, _Rest} =string:to_float(Body),
    Quote.
    
