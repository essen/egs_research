#!/bin/sh
rm -rf out/
erl -noshell -noinput -sname egs_research -pa ebin -pa deps/*/ebin -run egs_research start -run init stop
