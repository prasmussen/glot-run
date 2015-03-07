-module(docker_url).

-export([
    container_create/1,
    container_start/2,
    container_remove/3,
    container_attach/3
]).

container_create(BaseUrl) ->
    hackney_url:make_url(BaseUrl, <<"/containers/create">>, []).

container_start(BaseUrl, Id) ->
    hackney_url:make_url(BaseUrl, <<"/containers/", Id/binary, "/start">>, []).

container_remove(BaseUrl, Id, Params) ->
    hackney_url:make_url(BaseUrl, <<"/containers/", Id/binary>>, Params).

container_attach(BaseUrl, Id, Params) ->
    hackney_url:make_url(BaseUrl, <<"/containers/", Id/binary, "/attach">>, Params).
