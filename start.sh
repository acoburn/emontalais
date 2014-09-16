#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname acdc_prod \
    -config config/acdc.config \
    -detached \
    -s acdc \
    -s reloader
