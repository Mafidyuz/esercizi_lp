-module(hypercube).
%-export([create/0, loop/2]).
-compile(export_all).

n_diff([], []) -> 0;
n_diff([H1|T1], [H1|T2]) -> n_diff(T1, T2);
n_diff([_|T1], [_|T2]) -> 1 + n_diff(T1, T2).

create() -> 
    Greys = ["0000", "0001", "0011", "0010", "0110", "0111", "0101", "0100", "1100", "1101", "1111", "1110", "1010", "1011", "1001", "1000"],
    [put({pid, G}, spawn(hypercube, loop, [G, [L || L <- Greys, n_diff(L,G) == 1]])) || G <- Greys], ok.
    

loop(Label, Neighbors) ->
    io:format("The process labeled ~p just started, my neighbors are ~p.~n", [Label, Neighbors]),
    receive
        a -> a
    end.