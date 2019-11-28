%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Write a program that will create N processes connected in a ring. Once started, these processes will send M number of messages around the ring and then terminate gracefully when they receive a quit message. You can start the ring with the call ring:start(M, N, Message).

%There are two basic strategies to tackling this exercise. The first one is to have a central process that sets up the ring and initiates sending the message. The second strategy consists of the new process spawning the next process in the ring. With this strategy, you have to find a method to connect the first process to the second process.

%Try to solve the exercise in both manners. Note, when writing your program, make sure your code has many io:format statements in every loop iteration; this will give you a complete overview of what is happening (or not happening) and should help you solve the exercise.

-module(es_3).
-export([start/3, start1/3, loop/0]).

%central process
start(M, N, Message) -> 
    [H|T] = [spawn(es_3, loop, [])||_ <- lists:seq(1,N)],
    H ! {send, [H|T], T, M-1, Message}.

%recursive spawning
start1_aux(M, 0, Message, [H|T]) -> 
    Pid = spawn(es_3, loop, []),
    H ! {send, [H|T] ++ [Pid], T++[Pid], M-1, Message};

start1_aux(M, N, Message, Pids) -> 
    start1_aux(M, N-1, Message, Pids ++ [spawn(es_3, loop, [])]).

start1(M, N, Message) -> start1_aux(M, N-1, Message, []).

%loop function
loop() ->
    receive
        {send, _, _, 0, Message} -> io:format("~p: ~p ~n",[self(), Message]);
        {send, Pids, [], M, Message} -> [H|_] = Pids, io:format("~p: ~p ~n",[self(), Message]),  H ! {send, Pids, Pids, M-1, Message}, loop();
        {send, Pids, [H], M, Message} -> io:format("~p: ~p ~n",[self(), Message]),  H ! {send, Pids, Pids, M-1, Message}, loop();
        {send, Pids, [H|T], M, Message} -> io:format("~p: ~p ~n", [self(), Message]) , H ! {send, Pids, T, M-1, Message}, loop()
    end.