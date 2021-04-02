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
    application:start(inets),
    httpc:set_options([ 
                        { max_sessions, 5 } %% Maximum number of persistent connections to a host. Default is 2.
                      ]),
    application:start(eredis),    
    application:start(sasl),
    application:start(erprice),
    %% Boot also the erprice_ge
    {ok, GenServer}=gen_server:start_link(erprice_quote,[],[]),
    error_logger:info_msg("Erprice quote server: ~p",[GenServer]),
    %% Boot strategist guys
    gen_event:notify(erprice_ge, {kickoff, strategist, GenServer}),
    GenServer.


banner()->
    io:format("~n---------------------------------------------------"),
    io:format("~n--------------- Started Er Pricer App -------------"),
    io:format("~n- V0.0.0                                          -"),
    io:format("~n").

stop(_State) ->
	ok.
