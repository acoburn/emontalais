#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -config config/logging.config \
    -config config/emontalais.config \
    -sname emontalais_dev \
    -s emontalais \
    -s reloader
