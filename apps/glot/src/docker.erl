-module(docker).

-export([
    container_create/1,
    container_start/1,
    container_remove/1,
    container_attach/1,
    container_send/2
]).

container_create(Configuration) ->
    {ok, 201, _, Client} = hackney:post(
        docker_url:container_create(config:docker_api_url()),
        [{<<"Content-Type">>, <<"application/json">>}],
        jsx:encode(Configuration),
        []
    ),
    {ok, Data} = hackney:body(Client),
    jsx:decode(Data).

container_start(Id) ->
    {ok, 204, _, _} = hackney:post(
        docker_url:container_start(config:docker_api_url(), Id),
        [{<<"Content-Type">>, <<"application/json">>}],
        <<"">>,
        []
    ),
    ok.

container_remove(Id) ->
    {ok, 204, _, _} = hackney:delete(
        docker_url:container_remove(config:docker_api_url(), Id),
        [{<<"Content-Type">>, <<"application/json">>}],
        <<"">>,
        []
    ),
    ok.

container_attach(Id) ->
    {ok, Pid} = docker_attach_sup:start_child(),
    docker_attach:attach(Pid, Id),
    Pid.

container_send(Pid, Payload) ->
    docker_attach:send(Pid, Payload).
