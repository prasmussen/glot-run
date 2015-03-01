-module(admin_token_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_provided/2,
    is_authorized/2,
    resource_exists/2,
    delete_resource/2
]).

-record(state, {
    token
}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    http_util:log_request(Req),
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    Methods = [<<"DELETE">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, noop}
    ],
    {Handlers, Req, State}.

is_authorized(Req, State) ->
    http_auth:authorize_admin(Req, State).

resource_exists(Req, State) ->
    {Token, _} = cowboy_req:binding(token, Req),

    case user_token:is_valid(Token) of
        true ->
            {true, Req, State#state{token=Token}};
        false ->
            {false, Req, State}
    end.

delete_resource(Req, State=#state{token=Token}) ->
    user_token:delete(Token),
    {true, Req, State}.
