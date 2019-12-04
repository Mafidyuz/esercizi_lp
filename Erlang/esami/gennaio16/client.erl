-module(client).
-compile(export_all).

convert(from, From, to, To, X) -> 
    get({conv, From}) ! {convert, self(), To, X},
    receive
        {conversion, N} -> io:format("~p°~p are equivalent to ~p°~p ~n", [X, From, N, To])
    end.