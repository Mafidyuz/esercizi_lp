% To reverse the order of the characters in a string is quite trivial but when the length of the string grows trivial could means extremely slow. To decompose the input string in shorter strings and to join back the inverted strings in order to keep the algorithm working always on a short input is a good idea to fast the process.
% 
% Write a master-slave (see the previous exercise) distributed system where:
% 
%     a server process (MasterProcess) provides a service called long_reverse_string() that given a very large string (≥ 1000 characters) returns the reversed string;
%     the service long_reversed_string() decomposes the input w into 10 strings w⁰, ..., w⁹ with the following lengths (# represents the operator length of a string, and n=#w%10): #w⁰=...=#wⁿ=#w/10+1 e #wⁿ⁺¹=...=#w⁹=#w/10 and forwards to 10 distinct actors (SlaveProcess) to reverse the 10 substrings w⁰, ..., w⁹ (service reverse_string()) and joins the 10 results.
%     the client process (ClientProcess) just ask for the service on a given string.
% 
% When done with the exercise try to relax the constraint on the number of substrings from ten to a generic M passed as an input to the long_reversed_string service.

-module(drs).
-compile(export_all).

start() -> register(master, spawn(drs, master_process, [self()])).

master_process(Pid) -> 
    [put({slave, N}, spawn_link(drs, slave_loop, [])) || N <- lists:seq(1,10)],
    master_loop(Pid, 0).

master_loop(Pid, 10) -> 
    Revs = [get({str, N}) || N <- lists:seq(1,10), get({str, N}) =/= undefined],
    Pid ! lists:foldl(fun(X, Y) -> X ++ Y end, "",  Revs),
    master_loop(Pid, 0);

master_loop(Pid, Nstrings) ->
    receive
        {reverse, S} -> 
            Subs = [string:substr(S,  N+1 , floor(string:length(S)/10) ) || N <- [round(X * floor(string:length(S)/10)) || X <- lists:seq(0,9)]],
            [get({slave, N}) ! {self(), {rev, N, lists:nth(N, Subs)}} || N <- lists:seq(1,10)],
            master_loop(Pid, Nstrings);
        {reversed, N, S} -> put({str, N}, S), master_loop(Pid, Nstrings + 1)
    end.

long_reverse_string(S) -> 
    master ! {reverse, S},
    receive
        Msg -> io:format("~p~n", [Msg])
    end.

slave_loop() ->
    receive 
        {From, {rev, N, S}} -> From ! {reversed, N, string:reverse(S)}, slave_loop()
    end.