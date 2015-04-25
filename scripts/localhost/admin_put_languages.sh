#!/bin/bash
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "python", "version": "latest", "image": "glot/python:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "go", "version": "latest", "image": "glot/golang:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "haskell", "version": "latest", "image": "glot/haskell:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "php", "version": "latest", "image": "glot/php:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "java", "version": "latest", "image": "glot/java:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "erlang", "version": "latest", "image": "glot/erlang:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "javascript", "version": "latest", "image": "glot/javascript:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "perl", "version": "latest", "image": "glot/perl:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "ruby", "version": "latest", "image": "glot/ruby:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "c", "version": "latest", "image": "glot/clang:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "cpp", "version": "latest", "image": "glot/clang:latest"}' localhost:8090/admin/languages
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"name": "bash", "version": "latest", "image": "glot/bash:latest"}' localhost:8090/admin/languages
