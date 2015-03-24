#!/bin/bash

ID=$1
curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X GET localhost:8090/admin/users/$1 | jq .
