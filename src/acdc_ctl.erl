-module(acdc_ctl).

-export([stop/1]).

stop([Node]) ->
    Res1 = rpc:call(Node, acdc, stop, []),
    io:format("Stopping ~p: ~p~n", [Node, Res1]),
    ok = rpc:call(Node, init, stop, []).

