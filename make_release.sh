#!/bin/bash

rm -rf _rel
relx -c config/relx.config

cd _rel
tar -czf glot.tar.gz glot
cd -
