-module(emontalais_resource).
-export([
    init/1,
    allowed_methods/2,
    content_types_provided/2,
    delete_resource/2,
    test/2,
    to_json/2,
    to_xml/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

-spec allowed_methods(wrq:reqdata(), term()) ->
    {[Method], wrq:reqdata(), term()}
    when Method :: 'GET' | 'HEAD' | 'PUT' | 'POST' | 'DELETE' | 'OPTIONS'.
allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'POST', 'PUT', 'DELETE'], ReqData, Context}.


-spec content_types_provided(wrq:reqdata(), term()) ->
    {iodata(), wrq:reqdata(), term()}.
content_types_provided(ReqData, Context) ->
    {[{"application/xml", to_xml}, {"application/json", to_json}], ReqData, Context}.


-spec delete_resource(wrq:reqdata(), term()) ->
    {boolean(), req:reqdata(), term()}.
delete_resource(ReqData, State) ->
    {ok, Host} = application:get_env(emontalais, riak_host),
    {ok, Port} = application:get_env(emontalais, riak_pb_port),
    {ok, Riak} = riakc_pb_socket:start_link(Host, Port),
    Id = list_to_binary(wrq:path_info(id, ReqData)),
    riakc_pb_socket:delete(Riak, <<"fedora">>, Id),
    riakc_pb_socket:delete(Riak, <<"TN">>, Id),
    riakc_pb_socket:delete(Riak, <<"SMALL_SIZE">>, Id),
    riakc_pb_socket:delete(Riak, <<"MEDIUM_SIZE">>, Id),
    riakc_pb_socket:stop(Riak),
    {true, ReqData, State}.

%%%%%%
%% Testing here
uri_to_pid(Bin) ->
    "info:fedora/" ++ Pid = binary_to_list(Bin),
    list_to_binary(Pid).

-spec test(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
test(ReqData, State) ->
    Id = wrq:path_info(id, ReqData),
    {results, Results} = fedora:sparql(fedora:init(), 
        "SELECT * WHERE { ?obj <fedora-rels-ext:isPartOf> <fedora:" ++ Id ++ "> . }"),
    [H | _] = [uri_to_pid(X) || [{<<"obj">>, X}] <- Results],
    {H, ReqData, State}.

%% End testing
%%%%%%%

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    {ok, Host} = application:get_env(emontalais, riak_host),
    {ok, Port} = application:get_env(emontalais, riak_pb_port),
    {ok, Riak} = riakc_pb_socket:start_link(Host, Port),
    Id = wrq:path_info(id, ReqData),
    {ok, {riakc_obj, _Bucket, _Key, _Vclock, [{_Metadata, Value}], _UpdateMeta, _UpdateVal}} = riakc_pb_socket:get(Riak, <<"fedora">>, list_to_binary(Id)),
    riakc_pb_socket:stop(Riak),
    {binary_to_list(Value), ReqData, State}.

-spec to_xml(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_xml(ReqData, State) ->
    {xml, Xml} = wrap_xml(compose_xml(fedora:init(), wrq:path_info(id, ReqData))),
    {Xml, ReqData, State}.

wrap_xml({ok, Xml}) ->
    {xml, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
        "<acdc:acdc xmlns:acdc=\"http://acdc.amherst.edu/relationships#\">" ++ Xml ++ "</acdc:acdc>"}.

compose_xml(Fedora, Id) ->
    {ok, Profile} = fedora_xml:object(Fedora, Id),
    {ok, Relationships} = fedora_xml:datastream(Fedora, Id, "RELS-EXT"),
    {ok, Rights} = fedora_xml:datastream(Fedora, Id, "rightsMetadata"),
    case fedora_xml:datastream(Fedora, Id, "MODS") of
        {ok, Mods} ->
            {ok, Profile ++ Relationships ++ Rights ++ Mods};
        _ ->
            {ok, Profile ++ Relationships ++ Rights}
    end.


