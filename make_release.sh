#!/bin/bash

rm -rf _rel
relx -c config/relx.config

cd _rel
tar -czf glot-run.tar.gz glot
cd -
