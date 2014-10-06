#!/bin/sh

. ./env.sh

EXEC_CMD="sh -c"

$EXEC_CMD "$ERL -pa ebin deps/*/ebin \
    -sname ctl_emontalais \
    -config config/emontalais.config \
    -noshell \
    -s emontalais_ctl stop $ERLANG_NODE@$HOST \
    -s init stop"

rm -f $PIDFILE
