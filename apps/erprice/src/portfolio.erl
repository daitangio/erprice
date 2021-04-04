-module(portfolio).

-compile(native).
-export([new/1,buy/4]).

%% @doc Basic portfolio management


%% Create a new empty portfolio with the Amount specified. The amount is used to buy
new(Amount)->
    #{ amount => Amount }.

buy(Portfolio, Ticker, Quantity, Price)->
    Amount=maps:get(amount,Portfolio),
    PaidPremium=(Quantity*Price),
    RestAmount= Amount - PaidPremium,
    Order=case maps:is_key(Ticker,Portfolio) of
              true ->
            
                  %% Already here
                  PrevOrder=maps:get(Ticker,Portfolio),
                  PrevQuantity=maps:get(quantity,PrevOrder),
                  NewQuantity= PrevQuantity +Quantity,
                  %% Wrong Resee...
                  NewAvgPrice=maps:get(avgPrice,PrevOrder)+Price,
                  NewPremium=maps:get(val,PrevOrder)+PaidPremium,
                  #{ ticker => Ticker, quantity => NewQuantity, avgPrice => NewAvgPrice, val => NewPremium }
            
            ;
        false ->
            %% First ticker...
            #{ ticker => Ticker, quantity => Quantity, avgPrice => Price, val => PaidPremium }
    end,
    NewPortfolioOrder = maps:put(Ticker,Order,Portfolio),
    NewPortfolio      = maps:put(amount, RestAmount, NewPortfolioOrder),
    NewPortfolio.
