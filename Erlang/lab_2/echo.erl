-module(echo).
-compile(export_all).

start() -> register(echo, spawn(echo, loop, [])).

loop() -> 
    receive
        {From, stop} -> stop, From ! stopped;
        {From, Msg} -> io:format("Il messaggio è: ~p~n", [Msg]), From ! ok_printed, loop()
    end.

print(Pid, Msg) -> io:format("~p~n", [Msg]), echo ! {Pid, Msg}.

stop(Pid) -> echo ! {Pid, stop}. 