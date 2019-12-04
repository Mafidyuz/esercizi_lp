-module(ring).
%-export([send_message/1, send_message/2, stop/0]).
-compile(export_all).


start(N, L) -> 
    Pids = [spawn(ring, init, [Num, Fun]) || {Num, Fun} <- lists:zip(lists:seq(1,N),L)],
    [P ! {connect, Next} || {P, Next} <- lists:zip(Pids, tl(Pids) ++ [hd(Pids)])],
    put(npids, N),
    register(first, hd(Pids)).

init(Num, Fun) ->
    receive
        {connect, Next} -> loop(Num, Fun, Next)
    end.

loop(Num, Fun, Next) ->  
    receive
        {send_message, N, 1} ->  io:format("~p~n", [Fun(N)]), loop(Num, Fun, Next);
        {send_message, N, M} ->  Next ! {send_message, Fun(N), M-1}, loop(Num, Fun, Next);
        stop -> Next ! stop
    end.

send_message(N, M) -> first ! {send_message, N, get(npids) * M}.

send_message(N) -> send_message(N, 1).

stop() -> first ! stop.