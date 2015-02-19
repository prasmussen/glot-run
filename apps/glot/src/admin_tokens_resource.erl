-module(admin_tokens_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    %is_authorized/2,
    list_tokens/2,
    accept_put/2
]).

-define(INVALID_JSON, <<"Invalid json">>).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, []}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"PUT">>],
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, accept_put}
    ],
    {Handlers, Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, list_tokens}
    ],
    {Handlers, Req, State}.

%is_authorized(Req, State) ->
%    case http_auth:is_authorized_admin(Req, State) of
%        ok -> {true, Req};
%        Unauthorized -> Unauthorized
%    end.

list_tokens(Req, State) ->
    Tokens = token:list(),
    {jsx:encode(Tokens), Req, State}.

accept_put(Req, State) ->
    decode_body(fun save_token/3, Req, State).

save_token(Data, Req, State) ->
    Token = proplists:get_value(<<"token">>, Data),
    lager:info("Token: ~p", [Token]),
    token:save(Token),
    {true, Req, State}.

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
