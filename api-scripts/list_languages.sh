#!/bin/bash
curl -sv localhost:8090/languages/ | jq .
