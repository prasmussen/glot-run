-module(docker).

-export([
    container_create/1,
    container_start/1,
    container_remove/1,
    container_attach/1,
    container_detach/2,
    container_send/2
]).

container_create(Configuration) ->
    {ok, 201, _, Client} = hackney:post(
        docker_url:container_create(config:docker_api_url()),
        [{<<"Content-Type">>, <<"application/json">>}],
        jsx:encode(Configuration),
        [{pool, default}]
    ),
    {ok, Data} = hackney:body(Client),
    proplists:get_value(<<"Id">>, jsx:decode(Data)).

container_start(Id) ->
    {ok, 204, _, Client} = hackney:post(
        docker_url:container_start(config:docker_api_url(), Id),
        [{<<"Content-Type">>, <<"application/json">>}],
        <<"">>,
        [{pool, default}]
    ),
    hackney:skip_body(Client),
    ok.

container_remove(Id) ->
    Url = docker_url:container_remove(config:docker_api_url(), Id, [
        {v, true}, {force, true}
    ]),
    {ok, 204, _, Client} = hackney:delete(
        Url,
        [{<<"Content-Type">>, <<"application/json">>}],
        <<"">>,
        [{pool, default}]
    ),
    hackney:skip_body(Client),
    ok.

container_attach(Id) ->
    {ok, Pid} = docker_attach_sup:start_child(),
    docker_attach:attach(Pid, Id),
    Pid.

container_detach(Pid, Reason) ->
    docker_attach:detach(Pid, Reason).

container_send(Pid, Payload) ->
    docker_attach:send(Pid, Payload).
