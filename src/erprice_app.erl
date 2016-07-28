-module(erprice_app).
-author("giovanni.giorgi@gioorgi.com").
-behaviour(application).

-export([start/2, s/0]).
-export([stop/1]).
-export([banner/0]).

start(_Type, _Args) ->
    banner(),
    %% Dispatch=cowboy_router:compile(
    %%            [
    %%             {'_',[{"/", hello_handler, []}]}
    %%            ]),
    %% {ok, _Pid} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    %%     [{env, [{dispatch, Dispatch}]}]
    %% ),
    %% erprice_quote:start_link(),
    erprice_sup:start_link().

%% @doc convenience method to start basic services
s() ->    
    %% SSL needed by gen_smtp   
    application:start(inets),
    application:start(eredis),    
    application:start(sasl),    
    %% Resee: tree for gen_smtp
    %% application:start(crypto),
    %% application:start(asn1),   
    %% application:start(public_key),
    %% application:start(ssl),
    %% application:start(gen_smtp),
    %%% Go
    application:start(erprice),
    observer:start(),
    %% erprice_quote:start_link(),
    {ok, GenServer}=gen_server:start_link(erprice_quote,[],[]),
    erprice_quote:dropPercentScan(GenServer,0.05, 
                                  [ 
                                    {"ORCL","NY"},
                                    {"SGR","MI"},
                                    {"TRN","MI"},
                                    {"BMPS","MI"},
                                    {"ENEL","MI"}, 
                                    {"AAPL","NY"},
                                    {"RHT","NY"},
                                    {"ADBE","NY"}, %% Adobe, Nasdaq
                                    {"AMZN","NY"}
                                  ]),
    GenServer.


banner()->
    io:format("~n---------------------------------------------------"),
    io:format("~n--------------- Started Er Pricer App -------------"),
    io:format("~n- V0.0.0                                          -"),
    io:format("~n").

stop(_State) ->
	ok.
