#!/bin/bash

curl -sv -H "Authorization: Token C32BE915-38E8-486C-96DB-28C71EA87E42" -H 'Content-type: application/json' -X POST -d '[{"name": "foo/bar.sh", "content": "echo \"hello\"\n"}]' localhost:8090/languages/bash/latest | jq .
