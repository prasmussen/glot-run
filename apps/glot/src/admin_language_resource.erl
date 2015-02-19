-module(admin_language_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    %is_authorized/2,
    resource_exists/2,
    delete_resource/2,
    get_language/2
]).

-record(state, {
    id
}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"DELETE">>],
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, noop}
    ],
    {Handlers, Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, get_language}
    ],
    {Handlers, Req, State}.

% TODO
%is_authorized(Req, State) ->
%    case http_auth:is_authorized_admin(Req, State) of
%        ok -> {true, Req};
%        Unauthorized -> Unauthorized
%    end.

resource_exists(Req, State) ->
    {Id, _} = cowboy_req:binding(id, Req),

    case language:exists(Id) of
        true ->
            {true, Req, State#state{id=Id}};
        false ->
            {false, Req, State}
    end.

get_language(Req, State=#state{id=Id}) ->
    Language = language_tuple_to_map(language:get(Id)),
    {jsx:encode(Language), Req, State}.


delete_resource(Req, State=#state{id=Id}) ->
    language:delete(Id),
    {true, Req, State}.

language_tuple_to_map({Id, Language, Version, Image}) ->
    #{
        id => Id,
        language => Language,
        version => Version,
        image => Image
    }.
