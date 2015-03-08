-module(admin_languages_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    is_authorized/2,
    list_languages/2,
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
        {{<<"application">>, <<"json">>, '*'}, list_languages}
    ],
    {Handlers, Req, State}.

is_authorized(Req, State) ->
    http_auth:authorize_admin(Req, State).

list_languages(Req, State) ->
    Languages = [language_tuple_to_map(X) || X <- language:list()],
    {jsx:encode(Languages), Req, State}.

accept_put(Req, State) ->
    http_util:decode_body(fun save_language/3, Req, State).

save_language(Data, Req, State) ->
    % TODO: Ensure that all values are defined, i.e. not undefined
    {Name, Vsn, Image} = proplist_to_language_tuple(Data),
    Id = language:save(Name, Vsn, Image),
    LanguageMap = language_tuple_to_map(language:get(Id)),
    Req2 = cowboy_req:set_resp_body(jsx:encode(LanguageMap), Req),
    {true, Req2, State}.

language_tuple_to_map({Id, Name, Version, Image}) ->
    BaseUrl = config:base_url(),
    #{
        id => Id,
        name => Name,
        version => Version,
        image => Image,
        url => <<BaseUrl/binary, "/admin/languages/", Id/binary>>
    }.

proplist_to_language_tuple(Data) ->
    {
        proplists:get_value(<<"name">>, Data),
        proplists:get_value(<<"version">>, Data),
        proplists:get_value(<<"image">>, Data)
    }.
