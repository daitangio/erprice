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

simple_url_request_test()->    
    {ok, {{Version, 200, _ReasonPhrase}, Headers, Body}} =
        httpc:request(get, {"http://gioorgi.com", []}, [], []),
    lager:info("Version and Headers: ~p /// ~p ",[Version,Headers]),
    lager:info("Request Body:  ~p",[Body]).

%% Es di url di quotazione
%% http://www.milanofinanza.it/quotazioni/dettaglio/snam-2ae0363
%% http://finanza-mercati.ilsole24ore.com/quotazioni.php?QUOTE=!SRG.MI
%% Va fatto il parsing di qualcosa come
%%	<div class="toptitolo"><h1>Snam  - SRG<br><span class="isinTop">Codice ISIN <b>IT0003153415</b></span></h1></div><div class="databox"><table border="0" cellpadding="0" cellspacing="0"><tbody><tr><td class="desc" style="width:60px;">VAR%</td><td class="data varup"><nobr>+0.87&nbsp;<img style="vertical-align:middle;" alt="" src="/images/big_change_up.gif"></nobr></td></tr></tbody></table></div><div class="databox-b"></div><div class="databox"><table border="0" cellpadding="0" cellspacing="0"><tbody><tr><td class="desc" style="width:60px;">ULTIMO<br>PREZZO</td><td class="data varup" style="color:#FFFFFF">5.22</td></tr></tbody></table></div>
%% La chiave è il <td class="data varup" style="color:#FFFFFF">5.22</td></tr></tbody></table></div>
   
                               		        
        

    
-endif.


