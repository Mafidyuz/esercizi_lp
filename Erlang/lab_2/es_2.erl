-module(es_2).
-compile(export_all).

start() -> register(store, spawn(fun() -> loop() end)).

store(Tag, Value) -> rpc({store, Tag, Value}).

lookup(Tag) -> rpc({lookup, Tag}).

rpc(Call) -> 
    store ! {self(), Call},
    receive
        Response -> Response
    end.

loop() ->
    receive
        {From, {store, Tag, Value}} -> put(Tag, Value), From ! ok, loop();
        {From, {lookup, Tag}} -> From ! get(Tag), loop()
    end.
