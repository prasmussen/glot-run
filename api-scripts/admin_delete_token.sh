#!/bin/bash
TOKEN=$1
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X DELETE "localhost:8090/admin/tokens/${TOKEN}" | jq .
