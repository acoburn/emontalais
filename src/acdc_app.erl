%% @author Mochi Media <dev@mochimedia.com>
%% @copyright acdc Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the acdc application.

-module(acdc_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for acdc.
start(_Type, _StartArgs) ->
    acdc_deps:ensure(),
    acdc_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for acdc.
stop(_State) ->
    ok.
