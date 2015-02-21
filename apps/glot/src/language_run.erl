-module(language_run).

-export([
    run/3
]).

run(Language, Version, Files) ->
    Image = language:get_image(Language, Version),
    Config = create_docker_config(Image),
    ContainerId = docker:container_create(Config),
    RemoveRef = remove_after(config:docker_run_timeout() + 5, ContainerId),
    docker:container_start(ContainerId),
    Pid = docker:container_attach(ContainerId),
    DetachRef = detach_timeout_after(config:docker_run_timeout(), Pid),
    Payload = prepare_payload(Language, Files),
    Res = docker:container_send(Pid, Payload),
    cancel_timer(DetachRef),
    cancel_timer(RemoveRef),
    docker:container_remove(ContainerId),
    Res.

remove_after(Seconds, ContainerId) ->
    {ok, Ref} = timer:apply_after(
        Seconds * 1000,
        docker,
        container_remove,
        [ContainerId]
    ),
    Ref.

detach_timeout_after(Seconds, Pid) ->
    {ok, Ref} = timer:apply_after(
        Seconds * 1000,
        docker,
        container_detach,
        [Pid, timeout]
    ),
    Ref.

cancel_timer(Ref) ->
    timer:cancel(Ref).

prepare_payload(Language, Files) ->
    jsx:encode(#{
        <<"language">> => Language,
        <<"files">> => Files
    }).

create_docker_config(Image) ->
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
        <<"Entrypoint">> => <<"">>,
        <<"Image">> => <<"">>,
        <<"Volumes">> => #{},
        <<"WorkingDir">> => <<"">>,
        <<"NetworkDisabled">> => true,
        %<<"MacAddress">> => <<"12:34:56:78:9a:bc">>,
        %<<"Memory">> => 0,
        %<<"MemorySwap">> => 0,
        %<<"CpuShares">> => 512,
        %<<"Cpuset">> => <<"0">>,
        <<"ExposedPorts">> => #{},
        <<"SecurityOpts">> => [<<"">>],
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
            <<"DnsSearch">> => [<<"">>],
            <<"ExtraHosts">> => null,
            <<"VolumesFrom">> => [],
            <<"CapAdd">> => [<<"NET_ADMIN">>],
            <<"CapDrop">> => [<<"MKNOD">>],
            <<"RestartPolicy">> => #{
                <<"Name">> => <<"">>,
                <<"MaximumRetryCount">> => 0
            },
            <<"NetworkMode">> => <<"bridge">>,
            <<"Devices">> => []
        }
    }.
