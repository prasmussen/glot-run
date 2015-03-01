#!/bin/bash
NAME=$1
curl -sv "localhost:8090/languages/${NAME}"
