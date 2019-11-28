-module(echo).
-compile(export_all).

start() -> 
    Pid = spawn_link(fun() -> loop() end),
    register(echo, Pid).

loop() -> 
    receive
        stop -> io:format("Closing terminal..."), exit(not_normal);
        {print, Msg} -> io:format("~p~n", [Msg]), loop()
    end.

print(Term) -> echo ! {print, Term}.

stop() -> echo ! stop.