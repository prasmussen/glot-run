-module(admin_tokens_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    is_authorized/2,
    list_tokens/2,
    accept_put/2
]).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    http_util:log_request(Req),
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

is_authorized(Req, State) ->
    http_auth:authorize_admin(Req, State).

list_tokens(Req, State) ->
    Tokens = user_token:list(),
    {jsx:encode(Tokens), Req, State}.

accept_put(Req, State) ->
    http_util:decode_body(fun save_token/3, Req, State).

save_token(Data, Req, State) ->
    Token = proplists:get_value(<<"token">>, Data),
    lager:info("Token: ~p", [Token]),
    user_token:save(Token),
    {true, Req, State}.
