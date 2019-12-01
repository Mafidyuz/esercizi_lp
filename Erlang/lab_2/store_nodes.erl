-module(store_nodes).
-compile(export_all).

start() -> register(store, spawn_link(store_nodes, loop, [])).

rpc(Q) -> 
    store ! {self(), Q}.

store(Tag, Value) -> 
    rpc({store, Tag, Value}),
    [rpc:call(Node, ?MODULE, storeNode, [Tag, Value]) || Node <- nodes()], ok.    

storeNode(Tag, Value) -> rpc({storeNode, Tag, Value}).

lookup(Tag) -> rpc({lookup, Tag}).

loop() -> 
    receive
        {From, {store, Tag, Value}} -> put(Tag, Value), From ! ok, loop();
        {From, {storeNode, Tag, Value}} -> put(Tag, Value), From ! ok, loop();
        {From, {lookup, Tag}} -> From ! get(Tag), loop()
    end.