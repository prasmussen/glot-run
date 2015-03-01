#!/bin/bash

curl -sv -H "Authorization: Token C32BE915-38E8-486C-96DB-28C71EA87E42" -H 'Content-type: application/json' -X POST -d '[{"name": "main.erl", "content": "\nmain(_) ->\n    io:format(foo:test()).\n"}, {"name": "foo/foo.erl", "content": "-module(foo).\n\n-export([\n    test/0\n]).\n\ntest() ->\n    \"test\".\n"}]' localhost:8090/languages/erlang/latest/run | jq .
