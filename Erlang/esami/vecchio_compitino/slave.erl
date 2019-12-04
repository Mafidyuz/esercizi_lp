-module(slave).
-compile(export_all).

wait(N) -> 
    receive after
        N -> master ! N 
    end.