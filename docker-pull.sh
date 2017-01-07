#!/bin/sh

set -e

export DOCKER_HOST="tcp://127.0.0.1:80"
export ADMIN_TOKEN="some-secret"
export RUN_HOST="localhost:8080"

docker -H $DOCKER_HOST pull glot/python
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "python", "version": "latest", "image": "glot/python:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/python:2
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "python", "version": "2", "image": "glot/python:2"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/clang
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "c", "version": "latest", "image": "glot/clang:latest"}' "http://$RUN_HOST/admin/languages"
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "cpp", "version": "latest", "image": "glot/clang:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/php
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "php", "version": "latest", "image": "glot/php:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/javascript
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "javascript", "version": "latest", "image": "glot/javascript:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/javascript:es6
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "javascript", "version": "es6", "image": "glot/javascript:es6"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/java
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "java", "version": "latest", "image": "glot/java:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/golang
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "go", "version": "latest", "image": "glot/golang:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/bash
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "bash", "version": "latest", "image": "glot/bash:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/ruby
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "ruby", "version": "latest", "image": "glot/ruby:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/rust
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "rust", "version": "latest", "image": "glot/rust:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/mono
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "csharp", "version": "latest", "image": "glot/mono:latest"}' "http://$RUN_HOST/admin/languages"
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "fsharp", "version": "latest", "image": "glot/mono:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/haskell
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "haskell", "version": "latest", "image": "glot/haskell:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/coffeescript
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "coffeescript", "version": "latest", "image": "glot/coffeescript:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/elixir
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "elixir", "version": "latest", "image": "glot/elixir:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/erlang
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "erlang", "version": "latest", "image": "glot/erlang:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/assembly
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "assembly", "version": "latest", "image": "glot/assembly:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/clojure
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "clojure", "version": "latest", "image": "glot/clojure:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/lua
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "lua", "version": "latest", "image": "glot/lua:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/scala
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "scala", "version": "latest", "image": "glot/scala:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/perl
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "perl", "version": "latest", "image": "glot/perl:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/nim
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "nim", "version": "latest", "image": "glot/nim:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/dlang
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "d", "version": "latest", "image": "glot/dlang:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/idris
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "idris", "version": "latest", "image": "glot/idris:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/ocaml
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "ocaml", "version": "latest", "image": "glot/ocaml:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/elm
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "elm", "version": "latest", "image": "glot/elm:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/julia
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "julia", "version": "latest", "image": "glot/julia:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/swift
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "swift", "version": "latest", "image": "glot/swift:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/perl6
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "perl6", "version": "latest", "image": "glot/perl6:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/ats
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "ats", "version": "latest", "image": "glot/ats:latest"}' "http://$RUN_HOST/admin/languages"
docker -H $DOCKER_HOST pull glot/groovy
curl -X PUT -H "Authorization: Token $ADMIN_TOKEN" -H "Content-Type: application/json" -d '{"name": "groovy", "version": "latest", "image": "glot/groovy:latest"}' "http://$RUN_HOST/admin/languages"
