-module(client).
-compile(export_all).

send_msg(M) -> 
    {controller, 'sif@mario'} ! M,
    receive
        {res, R} -> io:format("~p~n",[R])
    end.

is_prime(N) -> send_msg({self(), {new, N}}).

