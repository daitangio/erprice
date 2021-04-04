-module(yahoo_extractor2021).
%% To run it use
%% rebar3 eunit  -m yahoo_extractor2021


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

new_concat_test()->
    Ticker  = "AAPL",
    Concat= unicode:characters_to_list(["https://query1.finance.yahoo.com/v7/finance/download/",Ticker,"?interval=1d&events=history&includeAdjustedClose=true"],utf8),
    error_logger:info_msg(Concat).

yahoo_price_test()->
    Ticker  = "AAPL",
    CallUrl= unicode:characters_to_list(["https://query1.finance.yahoo.com/v7/finance/download/",Ticker,"?interval=1d&events=history&includeAdjustedClose=true"],utf8),
    case httpc:request(get, {CallUrl, []}, [], []) of
        {error, Reason}  ->
            error_logger:info_msg("TickerError: ~p ~p",[Ticker,Reason]),
            -1
                ;
        {ok, Result}->
            {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
            ResponseLines=string:split(Body,"\n",all),  
            ?assertEqual("Date,Open,High,Low,Close,Adj Close,Volume",lists:nth(1,ResponseLines)),
            QuoteLine=lists:nth(2,ResponseLines),
            QuoteValueString=lists:nth(5,string:split(QuoteLine,",",all)),
            Quote=list_to_float(QuoteValueString),
            error_logger:info_msg("Quote ~p",[Quote]),
            Quote
    end.    

-endif.