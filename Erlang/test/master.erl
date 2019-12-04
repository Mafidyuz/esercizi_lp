-module(master).
-compile(export_all).

start(N) -> register(master, spawn(master, loopmaster, [N])).

loopmaster(N) ->
    io:format("I'm the master, i'm about to spawn ~p slaves.~n", [N]),
    [spawn(slave, loopslave, [X]) || X <- lists:seq(1,N)],
    waitformessages().

waitformessages() ->
    receive
        {fromslave, N} -> io:format("This is a message from slave ~p~n", [N]), waitformessages()
    end.