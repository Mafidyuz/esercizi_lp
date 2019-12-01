-module(ms).
-compile(export_all).

%start(N) which starts the master and tell it to start N slave processes and registers the master as the registered process master.

start(N) -> 
    register(master, spawn(ms, create_master, [N])).

create_master(N) -> 
    process_flag(trap_exit, true),
    [put(X, spawn_link(ms, loop_slave, [])) || X<-lists:seq(1,N)],
    loop_master().

loop_master() ->
    receive
        {to_slave, Message, N} -> get(N) ! {message, Message, N}, loop_master();
        {'EXIT', Pid, N} -> io:format("Master killed slave ~p with pid ~p ~n", [N, Pid]), put(N, spawn_link(ms, loop_slave, [])), loop_master()
    end.

loop_slave() ->
    receive
        {message, die, N} -> io:format("Slave ~p is dying.~n", [N]),  exit(N);
        {message, Message, N} -> io:format("Slave ~p got message ~p~n", [N, Message]), loop_slave()
    end.

%to_slave(Message, N) which sends a message to the master and tells it to relay the message to slave N; the slave should exit (and be restarted by the master) if the message is die.

to_slave(Message, N) -> master ! {to_slave, Message, N}.