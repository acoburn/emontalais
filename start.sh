#!/bin/sh
ERLANG_NODE=acdc1
EXEC_CMD="sh -c"

check_start()
{
    epmd -names 2>/dev/null | grep -q " ${ERLANG_NODE%@*} " && {
        ps ux | grep -v grep | grep -q " $ERLANG_NODE " && {
            echo "ERROR: The emontalais node '$ERLANG_NODE' is already running."
            exit 4
        } || {
            ps ux | grep -v grep | grep -q beam && {
                echo "ERROR: The emontalais node '$ERLANG_NODE' is registered,"
                echo "       but no related beam process has been found."
                echo "Shutdown all other erlang nodes, and call 'epmd -kill'."
                exit 5
            } || {
                epmd -kill >/dev/null
            }
        }
    }
}

check_start
$EXEC_CMD "erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname $ERLANG_NODE \
    -config config/logging.config \
    -config config/acdc.config \
    -detached \
    -s acdc \
    -s reloader"
