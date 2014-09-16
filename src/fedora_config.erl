-module(fedora_config).

-export([get/0]).

get() ->
    {ok, Uri} = application:get_env(acdc, fedoraurl),
    {ok, User} = application:get_env(acdc, fedorauser),
    {ok, Pass} = application:get_env(acdc, fedorapass),
    {fedora, Uri, User, Pass}.


