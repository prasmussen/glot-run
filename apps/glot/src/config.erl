-module(config).

-export([
    environment/0,
    glot_version/0,
    glot_description/0,
    http_listen_ip/0,
    http_listen_port/0,
    languages_data_path/0,
    users_data_path/0,
    http_log_path/0,
    event_log_path/0,
    base_url/0,
    admin_token/0,
    max_output_size/0,
    docker_api_url/0,
    docker_run_timeout/0,
    docker_container_config/1
]).

environment() ->
    list_to_atom(os:getenv("API_ENVIRONMENT")).

glot_version() ->
    {ok, Vsn} = application:get_key(glot, vsn),
    list_to_binary(Vsn).

glot_description() ->
    {ok, Desc} = application:get_key(glot, description),
    list_to_binary(Desc).

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

users_data_path() ->
    filename:join(data_path(), "users.data").

http_log_path() ->
    filename:join(log_path(), "http.log").

event_log_path() ->
    filename:join(log_path(), "event.log").

base_url() ->
    list_to_binary(os:getenv("BASE_URL")).

admin_token() ->
    list_to_binary(os:getenv("ADMIN_TOKEN")).

max_output_size() ->
    list_to_integer(os:getenv("MAX_OUTPUT_SIZE")).

docker_api_url() ->
    list_to_binary(os:getenv("DOCKER_API_URL")).

docker_run_timeout() ->
    list_to_integer(os:getenv("DOCKER_RUN_TIMEOUT")).

docker_container_config(Image) ->
    Config = default_docker_config(),
    Config#{<<"Image">> => Image}.

default_docker_config() ->
    #{
        <<"Hostname">> => <<"glot-runner">>,
        <<"Domainname">> => <<"">>,
        <<"User">> => <<"glot">>,
        <<"AttachStdin">> => true,
        <<"AttachStdout">> => true,
        <<"AttachStderr">> => true,
        <<"Tty">> => false,
        <<"OpenStdin">> => true,
        <<"StdinOnce">> => true,
        <<"Env">> => null,
        <<"Cmd">> => [<<"/home/glot/runner">>],
        <<"Entrypoint">> => <<"/home/glot/runner">>,
        <<"Image">> => <<"">>,
        <<"Volumes">> => #{},
        <<"WorkingDir">> => <<"">>,
        <<"NetworkDisabled">> => true,
        %<<"MacAddress">> => <<"12:34:56:78:9a:bc">>,
        <<"Memory">> => 500000000,
        %<<"MemorySwap">> => 0,
        %<<"CpuShares">> => 512,
        %<<"Cpuset">> => <<"0">>,
        <<"ExposedPorts">> => #{},
        %<<"SecurityOpt">> => null,
        <<"HostConfig">> => #{
            <<"Binds">> => [],
            <<"Links">> => [],
            <<"LxcConf">> => #{
                <<"lxc.utsname">> => <<"docker">>
            },
            <<"PortBindings">> => #{},
            <<"PublishAllPorts">> => false,
            <<"Privileged">> => false,
            <<"Dns">> => [<<"8.8.8.8">>],
            %<<"DnsSearch">> => [<<"">>],
            <<"ExtraHosts">> => null,
            <<"VolumesFrom">> => [],
            <<"CapAdd">> => [<<"NET_ADMIN">>],
            <<"CapDrop">> => [<<"MKNOD">>],
            <<"RestartPolicy">> => #{
                <<"Name">> => <<"">>,
                <<"MaximumRetryCount">> => 0
            },
            <<"NetworkMode">> => <<"bridge">>,
            <<"Devices">> => [],
            <<"Ulimits">> => [
                #{<<"Name">> => <<"nofile">>, <<"Soft">> => 90, <<"Hard">> => 100},
                #{<<"Name">> => <<"nproc">>, <<"Soft">> => 90, <<"Hard">> => 100}
            ]
        }
    }.
