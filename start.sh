#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname acdc1 \
    -config config/logging.config \
    -config config/acdc.config \
    -detached \
    -s acdc \
    -s reloader
