-module(sleepsort).
-compile(export_all).

%era uno sleepsort: ogni attore veniva messo in wait per un tempo uguale al numero che doveva ordinare e lo restituiva al server. In questo modo i numeri arrivavano ordinati

start() -> register(master, spawn(sleepsort, master, [])).

master() -> 
    receive
        {sort, L} -> 
            [spawn(slave, wait, [N]) || N <- L],
            waitsort(length(L))
    end.

waitsort(0) -> io:format("~n"), master();

waitsort(N) ->
    receive
        X -> io:format("~p, ", [X]), waitsort(N-1)
    end.

sort(L) -> master ! {sort, L}.