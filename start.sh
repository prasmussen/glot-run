#!/bin/sh
export API_ENVIRONMENT="development"
export API_HTTP_LISTEN_IP="0.0.0.0"
export API_HTTP_LISTEN_PORT="8090"
export DATA_PATH="/tmp/glot/data/"
export LOG_PATH="/tmp/glot/log/"
export ADMIN_TOKEN="clumeterin"
export DOCKER_API_URL="http://10.0.0.182:2375"
export DOCKER_RUN_TIMEOUT="60"

cd `dirname $0`
exec erl -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -s glot
