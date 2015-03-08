-module(languages_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_provided/2,
    list_names/2
]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, []}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, list_names}
    ],
    {Handlers, Req, State}.

list_names(Req, State) ->
    Names = [format_language(X) || X <- language:list_names()],
    {jsx:encode(Names), Req, State}.

format_language(Name) ->
    BaseUrl = config:base_url(),
    #{
        name => Name,
        url => <<BaseUrl/binary, "/languages/", Name/binary>>
    }.
