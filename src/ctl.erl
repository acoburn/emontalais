-module(ctl).

-export([stop/0]).

stop() ->
    Res = rpc:call(acdc1@dam08, init, stop, []),
    io:format("~p~n", [Res]).

