-module(erprice_quote).
-compile([{parse_transform, lager_transform},export_all]).
-export([]).

%% Provide Adapter for getting quotes

%% Example: http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!ORCL.NY
%% The page is fat but the parsing is quite easy
%% The generic form is something like a nested databox / data varup like...
%% <div class="databox"><table border="0" cellpadding="0" cellspacing="0"><tbody><tr><td class="desc" style="width:60px;">ULTIMO<br>PREZZO</td><td class="data varup" style="color:#FFFFFF">5.22</td></tr></tbody></table></div>
extract24_quote(String)->
    [ _Orig |  Rest ]=re:replace(String,"databox.*td class=\"data.*\" style=\"color:#FFFFFF\">([0-9.]*)</td>.*","\\1"),
    [ Match | _ ] = Rest,
    %%lager:info("Quote for SNAM:~p",Match), 
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
    lager:debug("Calling ~p",[CallUrl]),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {CallUrl, []}, [], []),
    FloatQuote=extract24_quote(Body),
    FloatQuote.
