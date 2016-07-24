-module(erprice_app).
-author("giovanni.giorgi@gioorgi.com").
-behaviour(application).

-export([start/2]).
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
    erprice_quote:start_link(),

    erprice_sup:start_link().

banner()->
    io:format("~n---------------------------------------------------"),
    io:format("~n--------------- Started Er Pricer App -------------"),
    io:format("~n- V0.0.0                                          -"),
    io:format("~n").

stop(_State) ->
	ok.
