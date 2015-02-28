-module(config).

-export([
    environment/0,
    http_listen_ip/0,
    http_listen_port/0,
    languages_data_path/0,
    tokens_data_path/0,
    http_log_path/0,
    event_log_path/0,
    admin_token/0,
    docker_api_url/0,
    docker_run_timeout/0
]).

environment() ->
    list_to_atom(os:getenv("API_ENVIRONMENT")).

http_listen_ip() ->
    {ok, Addr} = inet:parse_address(os:getenv("API_HTTP_LISTEN_IP")),
    Addr.

http_listen_port() ->
    list_to_integer(os:getenv("API_HTTP_LISTEN_PORT")).

data_path() ->
    Path = os:getenv("DATA_PATH"),
    filelib:ensure_dir(Path),
    Path.

log_path() ->
    Path = os:getenv("LOG_PATH"),
    filelib:ensure_dir(Path),
    Path.

languages_data_path() ->
    filename:join(data_path(), "languages.data").

tokens_data_path() ->
    filename:join(data_path(), "tokens.data").

http_log_path() ->
    filename:join(log_path(), "http.log").

event_log_path() ->
    filename:join(log_path(), "event.log").

admin_token() ->
    list_to_binary(os:getenv("ADMIN_TOKEN")).

docker_api_url() ->
    list_to_binary(os:getenv("DOCKER_API_URL")).

docker_run_timeout() ->
    list_to_integer(os:getenv("DOCKER_RUN_TIMEOUT")).
