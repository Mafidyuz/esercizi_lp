-module(tempsys).
-compile(export_all).

startsys() ->
    FromCelsiusFunctions = [
        {'C', fun(X) -> X end},
        {'F', fun(X) -> X * 9/5 + 32 end},
        {'K', fun(X) -> X + 273.15 end},
        {'R', fun(X) -> (X + 273.15) * 9/5 end},
        {'De', fun(X) -> (100 - X) * 3/2 end},
        {'N', fun(X) -> X * 33/100 end},
        {'Re', fun(X) -> X * 4/5 end},
        {'Ro', fun(X) -> X * 21/40 + 7.5 end}
    ],
    OfCels = [{{ofCelsius, Conversion}, spawn(tempsys, ofCelsius, [Fun])} || {Conversion, Fun} <- FromCelsiusFunctions],
    ToCelsiusFunctions = [
        {'C' ,fun(X) -> X end},
        {'De' ,fun(X) -> -(X*2/3-100) end},
        {'F' ,fun(X) -> (X - 32) * 5/9 end},
        {'K' ,fun(X) -> X-273.15 end},
        {'N' ,fun(X) -> X*100/33 end},
        {'R' ,fun(X) -> (X-491.67)*5/9 end},
        {'Re' ,fun(X) -> X*5/4 end},
        {'Ro' ,fun(X) -> (X-7.5)*40/21 end}
    ],
    [put({conv, Conversion}, spawn(tempsys, startConv, [Fun, OfCels])) || {Conversion, Fun} <- ToCelsiusFunctions], ok.

startConv(Fun, OfCels) -> 
    [put(A,B) || {A,B} <- OfCels],
    loopConv(Fun).

loopConv(Fun) ->
    receive
        {convert,Client, To, X} -> get({ofCelsius, To}) ! {convert, Client, self(), Fun(X)}, loopConv(Fun);
        {converted, Client, X} -> Client ! {conversion, X}, loopConv(Fun)
    end.

ofCelsius(Fun) ->
    receive
        {convert, Client, From, X} -> From ! {converted, Client, Fun(X)}, ofCelsius(Fun)
    end.