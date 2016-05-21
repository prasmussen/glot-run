-module(language_run).

-export([
    run/3
]).

run(Language, Version, Data) ->
    Image = language:get_image(Language, Version),
    Config = config:docker_container_config(Image),
    log:event(<<"Create container from image ", Image/binary>>),
    ContainerId = docker:container_create(Config),
    RemoveRef = remove_after(config:docker_run_timeout() + 5, ContainerId),
    log:event(<<"Start container ", ContainerId/binary>>),
    docker:container_start(ContainerId),
    log:event(<<"Attach container ", ContainerId/binary>>),
    Pid = docker:container_attach(ContainerId),
    DetachRef = detach_timeout_after(config:docker_run_timeout(), Pid),
    Payload = prepare_payload(Language, Data),
    log:event([<<"Send payload to ">>, ContainerId, <<" via ">>, util:pid_to_binary(Pid)]),
    Res = docker:container_send(Pid, Payload),
    [cancel_timer(X) || X <- [DetachRef, RemoveRef]],
    log:event(<<"Remove container ", ContainerId/binary>>),
    docker:container_remove(ContainerId),
    log:event(<<"Returning result">>),
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

prepare_payload(Language, Data) ->
    jsx:encode(#{
        <<"language">> => Language,
        <<"files">> => proplists:get_value(<<"files">>, Data, []),
        <<"stdin">> => proplists:get_value(<<"stdin">>, Data, <<"">>),
        <<"command">> => proplists:get_value(<<"command">>, Data, <<"">>)
    }).
