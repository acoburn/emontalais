-module(digital_object).

-export([xml/2, json/2]).
-author("Aaron Coburn <acoburn@amherst.edu>").

xml(Fedora, Fid) ->
    {ok, Profile} = fedora_xml:object(Fedora, Fid),
    {ok, Relationships} = fedora_xml:datastream(Fedora, Fid, "RELS-EXT"),
    {ok, Rights} = fedora_xml:datastream(Fedora, Fid, "rightsMetadata"),
    case fedora_xml:datastream(Fedora, Fid, "MODS") of
        {ok, Mods} ->
            {ok, Profile ++ Relationships ++ Rights ++ Mods};
        _ ->
            {ok, Profile ++ Relationships ++ Rights}
    end.

json(Riak, Fid) ->
    {ok, {riakc_obj, _Bucket, _Key, _Vclock, [{_Metadata, Value}], _UpdateMeta, _UpdateVal}} = riakc_pb_socket:get(Riak, <<"fedora">>, list_to_binary(Fid)),
    {ok, binary_to_list(Value)}.

