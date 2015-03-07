#!/bin/bash

curl -sv -H "Authorization: Token C32BE915-38E8-486C-96DB-28C71EA87E42" -H 'Content-type: application/json' -X POST -d '[{"name": "foo/hello.hs", "content": "main :: IO ()\nmain = putStrLn \"hello world\"\n"}]' localhost:8090/languages/haskell/latest | jq .
