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
    Tokens = [token_tuple_to_map(X) || X <- user_token:list()],
    {jsx:encode(Tokens), Req, State}.

accept_put(Req, State) ->
    http_util:decode_body(fun save_token/3, Req, State).

save_token(Data, Req, State) ->
    TokenValue = proplists:get_value(<<"token">>, Data),
    Id = user_token:save(TokenValue),
    TokenMap = token_tuple_to_map(user_token:get(Id)),
    Req2 = cowboy_req:set_resp_body(jsx:encode(TokenMap), Req),
    {true, Req2, State}.

token_tuple_to_map({Id, Token}) ->
    BaseUrl = config:base_url(),
    #{
        id => Id,
        token => Token,
        url => <<BaseUrl/binary, "/admin/tokens/", Id/binary>>
    }.
