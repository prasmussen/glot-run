-module(languages_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_provided/2,
    list_languages/2
]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, []}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, list_languages}
    ],
    {Handlers, Req, State}.

list_languages(Req, State) ->
    Data = jsx:encode([
        #{name => <<"erlang">>},
        #{name => <<"haskell">>},
        #{name => <<"clojure">>},
        #{name => <<"rust">>},
        #{name => <<"go">>}
    ]),
    {Data, Req, State}.
