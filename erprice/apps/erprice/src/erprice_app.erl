%%%-------------------------------------------------------------------
%% @doc erprice public API
%% @end
%%%-------------------------------------------------------------------

-module(erprice_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erprice_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
