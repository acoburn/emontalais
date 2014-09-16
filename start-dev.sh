#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -config config/acdc.config \
    -sname acdc_dev \
    -s acdc \
    -s reloader
