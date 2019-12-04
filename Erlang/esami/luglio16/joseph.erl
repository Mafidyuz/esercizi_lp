-module(joseph).
-compile(export_all).

joseph(N, K) -> 
    Pids = [spawn(hebrew, start, [X, K, self()]) || X <- lists:seq(1,N)],
    [P ! {init, Next} || {P, Next} <- lists:zip(Pids, tl(Pids) ++ [hd(Pids)])],
    io:format("In a circle of ~p people, killing number ~p~n",[N,K]),
    hd(Pids) ! {kill, K},
    receive
        {sole_survivor, Survivor} -> io:format("Sole survivor: ~p~n", [Survivor])
    end.

