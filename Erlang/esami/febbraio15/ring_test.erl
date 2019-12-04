-module(ring_test).
-compile(export_all).


start(N) -> 
    Pids = [spawn(ring_test, init, [Num]) || Num <- lists:seq(1,N)],
    [P ! {connect, Next} || {P, Next} <- lists:zip(Pids, tl(Pids) ++ [hd(Pids)])],
    register(first, hd(Pids)).

init(Num) ->
    receive
        {connect, Next} -> loop(Num, Next)
    end.

loop(Num, Next) ->  
    receive
        {send_message, Msg, 1} -> io:format("Nodo ~p: ~p~n", [Num, Msg]), loop(Num, Next);
        {send_message, Msg, M} -> io:format("Nodo ~p: ~p~n", [Num, Msg]), Next !  {send_message, Msg, M-1}, loop(Num,Next)
    end.

send_message(Msg, M) -> first ! {send_message, Msg, M}.