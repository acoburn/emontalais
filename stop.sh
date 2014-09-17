#!/bin/sh
ERLANG_NODE=acdc1
HOST=dam08
PIDFILE=/var/run/emontalais/emontalais.pid
EXEC_CMD="sh -c"
ERL=erl

$EXEC_CMD "$ERL -pa ebin deps/*/ebin \
    -sname ctl_acdc \
    -config config/acdc.config \
    -noshell \
    -s acdc_ctl stop $ERLANG_NODE@$HOST \
    -s init stop"

rm -f $PIDFILE
