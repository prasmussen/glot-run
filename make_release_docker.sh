#!/bin/bash

# This script requires the following docker image to exist
#
# FROM erlang:latest
# MAINTAINER Petter Rasmussen "petter.rasmussen@gmail.com"
# 
# RUN mkdir /build
# VOLUME ["/build"]
# WORKDIR /build
# CMD "/build/make_release.sh"

docker run \
  --volume $(pwd):/build \
  --rm \
  prasmussen/erlang-build:latest
