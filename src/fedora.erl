-module(fedora).

-export([
    object/2,
    datastream/3,
    sparql/2,
    init/0]).

init() ->
    {ok, Uri} = application:get_env(emontalais, fedoraurl),
    {ok, User} = application:get_env(emontalais, fedorauser),
    {ok, Pass} = application:get_env(emontalais, fedorapass),
    {fedora, Uri, User, Pass}.


sparql({fedora, Uri, User, Password}, Query) ->
    {ok, {{_Vsn, Status, Msg}, _Headers, Body}} = httpc:request(get, {Uri ++ "risearch?type=tuples&lang=sparql&flush=true&format=json&query=" ++ http_uri:encode(Query), [{
                        "Authorization", "Basic " ++ base64:encode_to_string(User ++ ":" ++ Password)}]}, [], []),
    case Status of
        200 ->
            [{<<"results">>, Results}] = jsx:decode(list_to_binary(Body)),
            {results, Results};
        _ ->
            {error, Msg}
    end.

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



