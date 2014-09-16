-module(digital_object).

-export([xml/2, json/2]).
-author("Aaron Coburn <acoburn@amherst.edu>").

xml(Fedora, Pid) ->
    {ok, Profile} = fedora_xml:object(Fedora, Pid),
    {ok, Relationships} = fedora_xml:datastream(Fedora, Pid, "RELS-EXT"),
    {ok, Rights} = fedora_xml:datastream(Fedora, Pid, "rightsMetadata"),
    {ok, Mods} = fedora_xml:datastream(Fedora, Pid, "MODS"),
    {ok, Profile ++ Relationships ++ Rights ++ Mods}.

json(Fedora, Pid) ->
    {ok, "OK"}.
