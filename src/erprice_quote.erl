-module(erprice_quote).
%% Native ask for hipe
-compile([native,export_all]).
-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, code_change/3,terminate/2,handle_info/2]).

%% For eunit:
-export([extract24_quote/1,get24price/1]).

%% Provide Adapter for getting quotes
%% Master Behavior server

%% @doc how much parallel connections for getting quote?
-define(MAX_CONNECTIONS,2).

%% @doc delay between two consecutive requests on the same quote resource
-define(COLD_DOWN_MS,60000).

start_link()->
    R=gen_server:start_link({local, ?MODULE }, ?MODULE, [], []),
    %%sys:trace(?MODULE,true),    
    R.

init([]) ->
    %% See http://erlang.org/doc/reference_manual/expressions.html#id82225
    State= #{ max_connections => ?MAX_CONNECTIONS, coldown => ?COLD_DOWN_MS },
    {ok,State}.

handle_call(quote,_From, State) ->
    {reply,use_cast, State}.

handle_cast({quote, _Company,_Market},State)->
    NewState = State,
    {noreply,NewState, 10000}.

%% handle_info({'EXIT', _Pid, _Reason}, State) ->
%%     %% ..code to handle exits here..
%%     {noreply, State}.

handle_info(_UnknownMessage,State) ->
    {noreply, State}.

terminate(normal,_State)->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Example: http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!ORCL.NY
%% The page is fat but the parsing is quite easy
%% The generic form is something like a nested databox / data varup like...
%% <div class="databox"><table border="0" cellpadding="0" cellspacing="0"><tbody><tr><td class="desc" style="width:60px;">ULTIMO<br>PREZZO</td><td class="data varup" style="color:#FFFFFF">5.22</td></tr></tbody></table></div>
extract24_quote(String)->
    [ _Orig |  Rest ]=re:replace(String,"databox.*td class=\"data.*\" style=\"color:#FFFFFF\">([0-9.]*)</td>.*","\\1"),
    [ Match | _ ] = Rest,    
    %% Ok, this route is stupid but works
    [ Az ] = Match,
    Quote=list_to_float(bitstring_to_list(Az)),
    Quote.


%% Sole24 API
%% The API is based on a "ticker" parameters
%% http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!ENEL.MI
get24price(Ticker) ->
    %% Concat the ticker
    CallUrl =string:concat("http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!",Ticker),
    error_logger:info_msg("Calling ~p",[CallUrl]),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {CallUrl, []}, [], []),
    FloatQuote=extract24_quote(Body),
    FloatQuote.

getYahooPrice(Ticker) ->
    %% nab= name, ask, bid.
    CallUrl =string:concat("http://download.finance.yahoo.com/d/quotes.csv?f=a&s=",Ticker),
    error_logger:info_msg("Calling ~p",[CallUrl]),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {CallUrl, []}, [], []),
    { Quote, _Rest} =string:to_float(Body),
    Quote.
    
