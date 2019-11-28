-module(test).
%-export([]).
-compile(export_all).


start() -> spawn(test, calc, []).

calc() -> 
    receive
        {sum, A, B} -> A + B, calc();
        {stop} -> stop
    end.
