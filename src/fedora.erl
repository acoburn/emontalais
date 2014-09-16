-module(fedora).

-export([object/2, datastream/3, init/0]).

init() ->
    {ok, Uri} = application:get_env(acdc, fedoraurl),
    {ok, User} = application:get_env(acdc, fedorauser),
    {ok, Pass} = application:get_env(acdc, fedorapass),
    {fedora, Uri, User, Pass}.



object({fedora, Uri, User, Password}, Pid) ->
    {ok, {{_Vsn, Status, Msg}, _Headers, Body}} = httpc:request(get, {Uri ++ "objects/" ++ Pid ++ "?format=xml", [{
                        "Authorization", "Basic " ++ base64:encode_to_string(User ++ ":" ++ Password)}]}, [], []),
    case Status of
        200 ->
            {object, Body};
        404 ->
            {notfound, Msg};
        _ ->
            {error, Msg}
    end.

datastream({fedora, Uri, User, Password}, Pid, Ds) ->
    {ok, {{_Vsn, Status, Msg}, _Headers, Body}} = httpc:request(get, {Uri ++ "objects/" ++ Pid ++ "/datastreams/" ++ Ds ++ "/content", [{
                        "Authorization", "Basic " ++ base64:encode_to_string(User ++ ":" ++ Password)}]}, [], []),
    case Status of
        200 ->
            {datastream, Body};
        404 ->
            {notfound, Msg};
        _ ->
            {error, Msg}
    end.



