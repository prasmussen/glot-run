-module(root_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_provided/2,
    root/2
]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, []}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, root}
    ],
    {Handlers, Req, State}.

root(Req, State) ->
    BaseUrl = config:base_url(),
    Data = #{
        description => config:glot_description(),
        version => config:glot_version(),
        urls => #{
            admin => <<BaseUrl/binary, "/admin">>,
            languages => <<BaseUrl/binary, "/languages">>
        }
    },
    {jsx:encode(Data), Req, State}.

