%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc acdc.

-module(acdc).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the acdc server.
start() ->
    acdc_deps:ensure(),
    ensure_started(crypto),
    application:start(acdc).


%% @spec stop() -> ok
%% @doc Stop the acdc server.
stop() ->
    application:stop(acdc).
