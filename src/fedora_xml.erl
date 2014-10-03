-module(fedora_xml).

-export([object/2, datastream/3]).
-author("Aaron Coburn <acoburn@amherst.edu>").

object(Fedora, Pid) ->
    {object, Xml} = fedora:object(Fedora, Pid),
    strip_preamble(Xml).

datastream(Fedora, Pid, Ds) ->
    Res = fedora:datastream(Fedora, Pid, Ds),
    case Res of
        {datastream, Xml} ->
            strip_preamble(Xml);
        _ ->
            {notfound}
    end.

strip_preamble(Xml) ->
    PreambleStart = string:str(Xml, "<?xml"),
    PreambleEnd = string:str(Xml, "?>"),
    case (PreambleStart > 0) and (PreambleEnd > 0) of
        true ->
            {ok, string:sub_string(Xml, PreambleEnd + 2)};
        _ ->
            {ok, Xml}
    end.

