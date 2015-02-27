-module(language_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_provided/2,
    resource_exists/2,
    list_versions/2
]).

-record(state, {
    language
}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    http_util:log_request(Req),
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, list_versions}
    ],
    {Handlers, Req, State}.

resource_exists(Req, State) ->
    {Lang, _} = cowboy_req:binding(language, Req),

    case language:is_supported(Lang) of
        true ->
            {true, Req, State#state{language=Lang}};
        false ->
            {false, Req, State}
    end.

list_versions(Req, State=#state{language=Language}) ->
    Versions = language:list_versions(Language),
    {jsx:encode(Versions), Req, State}.
