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
    write_pid_file(),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    inets:stop(),
    delete_pid_file(),
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                routes:get(Req:get(path), Req);

            'DELETE' ->
                routes:delete(Req:get(path), Req);

            'PUT' ->
                routes:put(Req:get(path), Req);

            'POST' ->
                routes:post(Req:get(path), Req);

            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            case What of
                {badmatch, {notfound, _}} ->
                    error_logger:error_msg("Error: ~p ~p~n", [Req:get(method), Req:get(path)]),
                    Req:not_found();

                _ ->
                    Report = ["web request failed",
                              {path, Req:get(path)},
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

write_pid_file() ->
    {ok, Path} = application:get_env(acdc, pidfile),
    write_pid_file(os:getpid(), Path).

write_pid_file(Pid, PidFilename) ->
    case file:open(PidFilename, [write]) of
        {ok, Fd} ->
            io:format(Fd, "~s~n", [Pid]),
            file:close(Fd);
        {error, Reason} ->
            error_logger:error_report("Cannot write PID file ~s~nReason: ~p~n", [PidFilename, Reason])
    end.

delete_pid_file() ->
    {ok, Path} = application:get_env(acdc, pidfile),
    file:delete(Path).


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
