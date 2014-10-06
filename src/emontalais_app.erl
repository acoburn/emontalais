-module(emontalais_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    write_pid_file(),
    emontalais_sup:start_link().

stop(_State) ->
    delete_pid_file(),
    ok.

write_pid_file() ->
    {ok, Path} = application:get_env(emontalais, pidfile),
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
    {ok, Path} = application:get_env(emontalais, pidfile),
    file:delete(Path).


