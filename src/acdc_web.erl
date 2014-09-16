%% @author Aaron Coburn <acoburn@amherst.edu>
%% @copyright 2014 Amherst College

%% @doc Web server for acdc.

-module(acdc_web).
-author("Aaron Coburn <acoburn@amherst.edu>").
-export([start/1, stop/0, loop/1]).

%% External API

start(Options) ->
    {_DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
    inets:start(),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    inets:stop(),
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "api/object/" ++ Pid ->
                        case Req:get_header_value("accept") of
                            "application/xml" ->
                                {ok, Xml} = digital_object:xml(fedora:init(), Pid),
                                Req:respond({200, [{"Content-Type", "application/xml; charset=utf-8"}], 
                                                 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
                                                 "<acdc>" ++ Xml ++ "</acdc>"});
                            "application/json" ->
                                {ok, Json} = digital_object:json(fedora:init(), Pid),
                                Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}],
                                             Json});
                            _ ->
                                Req:respond({200, [{"Content-Type", "text/plain"}],
                                            "Yeah I got it, but how d'ya want it?"})
                        end;
                    _ ->
                        Req:not_found()
                end;

            'DELETE' ->
                Req:not_found();

            'PUT' ->
                Req:not_found();

            'POST' ->
                Req:not_found();

            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            case What of
                {badmatch, {notfound, _}} ->
                    Req:not_found();

                _ ->
                    Report = ["web request failed",
                              {path, Path},
                              {type, Type}, {what, What},
                              {trace, erlang:get_stacktrace()}],
                    error_logger:error_report(Report),
                    Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
            end
    end.

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
