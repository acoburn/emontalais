-module(routes).
-author("Aaron Coburn <acoburn@amherst.edu>").

-export([get/2, delete/2, post/2, put/2]).

get("/ping", Req) ->
    Req:respond({200, [{"Content-Type", "text/plain"}], "pong"});

get("/api/object/" ++ Fid, Req) ->
    handle_get_negotiate(Req:get_header_value("accept"), Fid, Req);

get(_Path, Req) -> Req:not_found().

delete(_Path, Req) -> Req:not_found().

put(_Path, Req) -> Req:not_found().

post(_Path, Req) -> Req:not_found().

% Internal API
handle_get_negotiate(Format, Fid, Req) when Format =:= "application/xml" ->
    {ok, Xml} = digital_object:xml(fedora:init(), Fid),
    Req:respond({200, [{"Content-Type", Format ++ "; charset=utf-8"}], 
                     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
                     "<acdc:acdc xmlns:acdc=\"http://acdc.amherst.edu/relationships#\">" ++ Xml ++ "</acdc:acdc>"});

handle_get_negotiate(Format, Fid, Req) when Format =:= "application/json" ->
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    {ok, Json} = digital_object:json(Pid, Fid),
    riakc_pb_socket:stop(Pid),
    Req:respond({200, [{"Content-Type", Format ++ "; charset=utf-8"}],
                 Json});

handle_get_negotiate(_Accept, _Fid, Req) ->
    Req:respond({200, [{"Content-Type", "text/plain"}],
                "Yeah I got it, but how d'ya want it?"}).
