-module(fedora_xml).

-export([object/2, datastream/3]).
-author("Aaron Coburn <acoburn@amherst.edu>").

object(Fedora, Pid) ->
    {object, Xml} = fedora:object(Fedora, Pid),
    {Doc, _Misc} = xmerl_scan:string(Xml, [{space, normalize}]),
    [_, Elems] = xmerl:export_simple([Doc], xmerl_xml),
    {ok, lists:flatten(Elems)}.

datastream(Fedora, Pid, Ds) ->
    {datastream, Xml} = fedora:datastream(Fedora, Pid, Ds),
    {Doc, _Misc} = xmerl_scan:string(Xml, [{space, normalize}]),
    [_, Elems] = xmerl:export_simple([Doc], xmerl_xml),
    {ok, lists:flatten(Elems)}.

