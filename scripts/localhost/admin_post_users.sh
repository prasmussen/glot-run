#!/bin/bash

curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X POST -d '{"token": "af4c0a23-7b42-4207-b61c-3430c9637055"}' localhost:8090/admin/users | jq .
