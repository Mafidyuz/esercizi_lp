-module(controller).
-compile(export_all).

is_prime_aux(N) -> length([X || X <- lists:seq(2, round(math:sqrt(N)) ), N rem X == 0]) == 0.

start(N) -> 
    Primes = [ X || X <- lists:seq(2,N), is_prime_aux(X)],
    Sieves = [spawn(sieve, sieve_loop, [P]) || P <- Primes ],
    register(controller, spawn(controller, controller_loop, [Sieves])).

controller_loop([H|T]) ->
    receive
        {From, {new, N}} -> io:format("You asked for ~p~n", [N]), H ! {pass, self(), N, H, T, From}, controller_loop([H|T]);
        {Client, true} -> Client ! {res, true}, controller_loop([H|T]);
        {Client, false} -> Client ! {res, false}, controller_loop([H|T])
    end.
