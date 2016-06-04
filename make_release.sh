#!/bin/bash
set -e

rebar3 compile
rebar3 release -c config/relx.config
root_path=$(pwd)

(
    cd _build/default/rel
    tar -czf ${root_path}/glot-run.tar.gz glot
)
