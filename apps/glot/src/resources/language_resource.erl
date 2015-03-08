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
    name
}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
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
    {Name, _} = cowboy_req:binding(name, Req),

    case language:is_supported(Name) of
        true ->
            {true, Req, State#state{name=Name}};
        false ->
            {false, Req, State}
    end.

list_versions(Req, State=#state{name=Name}) ->
    Versions = [format_version(Name, X) || X <- language:list_versions(Name)],
    {jsx:encode(Versions), Req, State}.

format_version(Name, Version) ->
    BaseUrl = config:base_url(),
    #{
        version => Version,
        url => <<BaseUrl/binary, "/languages/", Name/binary, "/", Version/binary>>
    }.
