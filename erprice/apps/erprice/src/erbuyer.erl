-module(erbuyer).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

init(_Args) ->
    error_logger:info_msg("Buyer handler ready to speculate!"),
    {ok, []}.


handle_event(Action, State) ->    
    case Action of
        {kickoff, strategist, GenServer } ->
            erprice_quote:dropScan(GenServer,[{"BMPS","MI",0.24}]),
            erprice_quote:dropPercentScan(GenServer,0.15, 
                                          [{"BMPS","MI"}]),
            erprice_quote:dropPercentScan(GenServer,0.05, 
                                          [ 
                                    {"ORCL","NY"},
                                            {"SGR","MI"},
                                            {"TRN","MI"},                            
                                            {"ENEL","MI"}, 
                                            {"AAPL","NY"},
                                            {"RHT","NY"},
                                            {"ADBE","NY"}, %% Adobe, Nasdaq
                                            {"AMZN","NY"}
                                          ]),
            error_logger:info_msg("Base monitor on");
            {drop, _PidCaller, Ticker,CurrentQuote} ->
            error_logger:info_msg("Drop notification... ~p := ~p",[Ticker,CurrentQuote]);
        {zero_watcher_reached, _GenServerPid} ->
            %% Eval stopping everything because nothing left to monitor by the way
            error_logger:info_msg("!! No more watcher... Strategy needed");
        _ ->
            error_logger:error_msg("Unknown notification:~p~n",[Action])
            
    end,
    {ok, State}.


handle_call(_Request, State) ->
    {ok,reply_ok,State}.

handle_info(Info, State) ->
    error_logger:info_msg("Unknown message: ~p ~p",[Info,State]),
    {ok,State}.

code_change(OldVsn, State, Extra) ->
    error_logger:info_msg("Code Change Requested: ~p ~p ~p",[OldVsn, State, Extra]),
    {ok, State}.


terminate(_Args, _State) ->
    ok.
