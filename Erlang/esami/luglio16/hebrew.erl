-module(hebrew).
-compile(export_all).

start(N, K, Master) ->
    receive
        {init, Next} -> loop(Next, N, K, Master)
    end.

loop(Next, N, K, Master) -> 
    receive
        {rip, From} when From == Next -> Next ! sole_survivor;
        {rip, From} -> From ! {new_next, Next}, Next ! {kill, K};
        sole_survivor -> Master ! {sole_survivor, N};
        {new_next, NewNext} -> loop(NewNext, N, K, Master);
        {kill, 2} -> Next ! {rip, self()}, loop(Next, N, K, Master);
        {kill, X} -> Next ! {kill, X-1}, loop(Next, N, K, Master)
    end.