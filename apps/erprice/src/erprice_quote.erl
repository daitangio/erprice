-module(erprice_quote).
%% Native ask for hipe? Win does not support it
%% -compile([ native,export_all]).
-compile([native, {hipe, [to_llvm]}]).
-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, code_change/3,terminate/2,handle_info/2]).
-export([start_link/0,watch/5, dropPercentScan/3, 
    dropScan/2,watchDrop/4 
]).
%% For eunit:
-export([extract24_quote/1,get24price/1]).

%% Gen server master watcher. 
%% TODO Custom error log http://erlang.org/documentation/doc-4.9.1/doc/design_principles/error_logging.html
%% 

%% how much parallel connections for getting quote?
%% -define(MAX_CONNECTIONS,2).

%% Mimimal delay between two requests
-define(MIN_COLD_DOWN_SECONDS,120).
%% Random delay range
-define(COLD_DOWN_RANDOM_SECONDS,60).

%%% BASIC API
%% @doc Spawn a child which will notify the price drop to the Gen Server
watch(GenServer, Company, Market,lessthen,Quote)->
    {spawned, NewPid}=gen_server:call(GenServer,{start_tracker,Company, Market,lessthen,Quote}),
    NewPid.

%% i.e. dropPercentScan(Pid,0.1, [ {"BMPS","MI"}])
%% It needs a quote to start. It is very slow because it a sequential step. Consider parallel spawn.
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


%%% PRIVATE SUPPORT
%% @doc Spawned by gen_server
watchDrop(Company,Market,Quote,GenServer)->
    %% error_logger:info_msg("Monitoring lessthen ~p",Company),
    CurrentQuote =  getQuote(Company,Market),
    %% error_logger:info_msg("Comparing ~p ~p < ~p", [Company,CurrentQuote,Quote]),
    if CurrentQuote < Quote  -> 
            error_logger:info_msg("Trigger Comparing ~p ~p < ~p", [Company,CurrentQuote,Quote]),
            %% Notify drop
            gen_event:notify(erprice_ge, {drop, self(), string:concat(string:concat(Company,"."),Market),CurrentQuote});
            %%gen_server:cast(GenServer, {self(),drop,Company,CurrentQuote});           
       true -> %% error_logger:info_msg(" Retrying... "),
            %% Sleep and reschedule.
            %% To avoid detection, we add also some random time inside it.
            SleepTimeMs=1000*(?MIN_COLD_DOWN_SECONDS) + 
                %% Random 10sec distrib
                +1000+((trunc(rand:uniform()* ?COLD_DOWN_RANDOM_SECONDS)) *1000),
            %% error_logger:info_msg(" Retrying...~p into ~p   ~p < ~p ", [ Company,SleepTimeMs,CurrentQuote,Quote ]),
            timer:sleep( SleepTimeMs  ) ,               
            watchDrop(Company,Market,Quote,GenServer)
    end.


    

%%% GEN SERVER Implementation
%%% Spawn sub-monitor children
%%% Children notify price drop/rise to gen_event subsystem
%%% When a Children die, the gen server get a notification and update its state
%%% It can also 


start_link()->
    {ok, GenServer}=gen_server:start_link({local, ?MODULE }, ?MODULE, [], []),
    GenServer.

init(_WhateverYouLike) ->
    {ok, _}=gen_event:start({local,erprice_ge}),
    %% The buyer
    gen_event:add_handler(erprice_ge,erbuyer,[]),
    %% TODO: Add a  watcher guy and kick it in the game
    State=#{ monitorCount => 0 },
    {ok,State}.

handle_call({start_tracker, Company, Market,lessthen,Quote},_From, State) ->    
    NewPid=spawn_monitor(?MODULE,watchDrop,[Company,Market,Quote,self()]),
    %% TODO Store pid infos...    
    #{ monitorCount := CurrentCount } = State,
    NewCount = CurrentCount+1,
    UpdatedState = State#{ monitorCount := NewCount },
    %% La chiave non può essere la company senno' si ha  un solo monitor per ticker
    NewState=maps:put(NewPid,#{
                        pid => NewPid,
                        ticker => string:concat(string:concat(Company,"."),Market),
                        quote => Quote,
                        logic => lessthen
                       },UpdatedState),
    error_logger:info_msg("Spawned sub-process for ~p drop Current Count: ~p ~nNewState ~p ",[Company,NewCount,NewState]),
    {reply,{spawned,NewPid}, NewState}.


%% Unused right now
handle_cast(_Unknown,State)->
    {noreply,State}.
%% handle_cast({_WorkerPid, drop, Company,CurrentQuote},State)->
%%     error_logger:info_msg("~n NOTIFY Drop on ~p ~p",[Company,CurrentQuote]),
%%     %% TODO: implement some notification action
%%     %% Also if the trigger must be rescheduled provide additional actions
%%     {noreply,State}.

handle_info(UnknownMessage,State) ->
    %%error_logger:error_msg("~n UNKNOWN MESSAGE: ~p",[UnknownMessage]),
    %% Try magic match:
    case UnknownMessage of
        {'DOWN',Ref,process,Pid,normal}->
            #{ monitorCount := CurrentCount } = State,
            DecCount = CurrentCount-1,
            UpdatedState = State#{ monitorCount := DecCount },
            NewState= maps:without([{Pid,Ref}],UpdatedState),
            error_logger:info_msg("Aah: Child just shutdown: ~p New State: ~p ~n  ",[Pid, NewState]),
            
            if
                DecCount == 0 ->
                    error_logger:info_msg("Notify zero watcher..."),
                    gen_event:notify(erprice_ge, 
                                     {zero_watcher_reached, self()});
                true -> nothing2do
            end,
            {noreply, NewState};
        _ ->
            error_logger:info_msg("Unknown message: ~p",[UnknownMessage]),
            {noreply,State}
    end.
    

terminate(Reason,State)->
    error_logger:error_msg("~n Terminate Abnormal request. Reason:~p Status: ~p",[Reason,State]),
    ok.
%% terminate(normal,_State)->
%%     ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%% Low level web service quote services

getQuote(Company,Market)->
    CurrentQuote = 
        case Market of
            ""    -> getYahooPrice(Company) ; %% shortcut 
            "NY"  -> getYahooPrice(Company) ;
            _Other ->
                getYahooPrice(string:concat(string:concat(Company,"."),Market))           
                %% get24price(string:concat(string:concat(Company,"."),Market))                        
        end,
    error_logger:info_msg("Q! ~p  ~p",[Company,CurrentQuote]),
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
    %%error_logger:info_msg("Bodyz: ~p", [Body]),
    FloatQuote=extract24_quote(Body),
    FloatQuote.

getYahooPrice(Ticker) ->   
    CallUrl= unicode:characters_to_list(["https://query1.finance.yahoo.com/v7/finance/download/",Ticker,"?interval=1d&events=history&includeAdjustedClose=true"],utf8),
    %%error_logger:info_msg("Calling ~p",[CallUrl]),    
    case httpc:request(get, {CallUrl, []}, [], []) of
        {error, Reason}  ->
            error_logger:info_msg("TickerError: ~p ~p Retring in 10s",[Ticker,Reason]),
            timer:sleep(10000),
            getYahooPrice(Ticker)
            %%-1
                ;
        {ok, Result}->
            {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
            %% Take the second line
            QuoteLine=lists:nth(2,string:split(Body,"\n",all)),
            QuoteValueString=lists:nth(5,string:split(QuoteLine,",",all)),
            Quote=list_to_float(QuoteValueString),
            %%error_logger:info_msg("Quote ~p",[QuoteValueString]),
            Quote
    end.
    
