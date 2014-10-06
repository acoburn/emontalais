-module(emontalais_ctl).

-export([stop/1]).

stop([Node]) ->
    Res1 = rpc:call(Node, emontalais, stop, []),
    io:format("Stopping ~p: ~p~n", [Node, Res1]),
    ok = rpc:call(Node, init, stop, []).

