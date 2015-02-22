-module(http_util).

-export([
    decode_body/3,
    add_cors_headers/1
]).

-define(INVALID_JSON, <<"Invalid json">>).

decode_body(F, Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case jsx:is_json(Body) of
        true ->
            Data = jsx:decode(Body),
            F(Data, Req2, State);
        false ->
            Payload = jsx:encode([{message, ?INVALID_JSON}]),
            {ok, Req3} = cowboy_req:reply(400, [], Payload, Req2),
            {halt, Req3, State}
    end.

add_cors_headers(Req) ->
    Headers = [
        {<<"access-control-allow-methods">>, <<"POST, OPTIONS">>},
        {<<"access-control-allow-origin">>, <<"*">>},
        {<<"access-control-allow-headers">>, <<"Content-Type">>}
    ],
    set_headers(Headers, Req).

set_headers(Headers, Req) ->
    lists:foldl(fun({Name, Value}, R) ->
        cowboy_req:set_resp_header(Name, Value, R)
    end, Req, Headers).
