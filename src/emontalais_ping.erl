-module(emontalais_ping).
-export([
    init/1,
    content_types_provided/2,
    to_text/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) -> {ok, undefined}.

-spec content_types_provided(wrq:reqdata(), term()) ->
    {iodata(), wrq:reqdata(), term()}.
content_types_provided(ReqData, Context) ->
    {[{"text/plain", to_text}], ReqData, Context}.

-spec to_text(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_text(ReqData, Context) ->
    {"pong", ReqData, Context}.

