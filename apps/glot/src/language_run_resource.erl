-module(language_run_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    is_authorized/2,
    allow_missing_post/2,
    resource_exists/2,
    accept_post/2
]).

-define(TIMEOUT_ERROR, <<"Code exceeded the maximum allowed running time">>).

-record(state, {
    language,
    version
}).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>],
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, accept_post}
    ],
    {Handlers, Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, noop}
    ],
    {Handlers, Req, State}.

is_authorized(Req, State) ->
    http_auth:authorize_user(Req, State).

allow_missing_post(Req, State) ->
    {false, Req, State}.

resource_exists(Req, State) ->
    lager:debug("Resource exists?"),
    {Lang, _} = cowboy_req:binding(language, Req),
    {Vsn, _} = cowboy_req:binding(version, Req),

    case language:is_supported(Lang, Vsn) of
        true ->
            {true, Req, State#state{language=Lang, version=Vsn}};
        false ->
            {false, Req, State}
    end.

accept_post(Req, State) ->
    http_util:decode_body(fun run_code/3, Req, State).

run_code(Data, Req, State=#state{language=Lang, version=Vsn}) ->
    lager:info("Data: ~p", [Data]),
    case language_run:run(Lang, Vsn, Data) of
        {ok, Res} ->
            {true, cowboy_req:set_resp_body(Res, Req), State};
        {error, timeout} ->
            Res = jsx:encode(#{<<"message">> => ?TIMEOUT_ERROR}),
            {false, cowboy_req:set_resp_body(Res, Req), State};
        {error, Msg} ->
            Res = jsx:encode(#{<<"message">> => Msg}),
            {false, cowboy_req:set_resp_body(Res, Req), State}
    end.
