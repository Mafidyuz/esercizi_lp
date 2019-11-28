-module(counting).
-compile(export_all).

count(_, []) -> 0;
count(E, [E|T]) -> 1 + count(E, T);
count(E, [_|T]) -> count(E, T).

elements([]) -> [];
elements([H|T]) -> [H] ++ elements(lists:filter(fun(X) -> X =/= H end, T)).

start() -> register(counting,  spawn(fun() -> loop([]) end)).

loop(List) -> 
    receive
        dummy1 -> io:format("Dummy 1~n"), loop([dummy1|List]);
        dummy2 -> io:format("Dummy 2~n"), loop([dummy2|List]);
        dummy3 -> io:format("Dummy 3~n"), loop([dummy3|List]);
        dummy4 -> io:format("Dummy 4~n"), loop([dummy4|List]);
        close -> io:format("Chiusura, la lista è ~p ~n", [[{X, count(X,List)}||X <- elements(List)]])
    end.

