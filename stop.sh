#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -sname acdc2 \
    -config config/acdc.config \
    -noshell \
    -s ctl stop \
    -s init stop
