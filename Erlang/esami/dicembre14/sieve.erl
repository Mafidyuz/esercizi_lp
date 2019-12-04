-module(sieve).
-compile(export_all).

sieve_loop(P) -> 
    receive
        {res, true, Controller, Client} -> Controller ! {Client, true}, sieve_loop(P);
        {res, false, Controller, Client} -> Controller ! {Client, false}, sieve_loop(P);
        {pass, Controller, N, First, [], Client} when N rem P =/= 0 -> First ! {res, true, Controller, Client}, sieve_loop(P);
        {pass, Controller, N, First, [H|T], Client} when N rem P =/= 0 -> H ! {pass, Controller, N, First, T, Client}, sieve_loop(P);
        {pass, Controller, P,    First, _, Client} -> First ! {res, true, Controller, Client}, sieve_loop(P);
        {pass, Controller, N,    First, _, Client} when N rem P == 0 -> First ! {res, false, Controller, Client}, sieve_loop(P)
    end.

