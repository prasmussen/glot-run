-module(images_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_provided/2,
    list_images/2
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
        {{<<"application">>, <<"json">>, '*'}, list_images}
    ],
    {Handlers, Req, State}.

list_images(Req, State) ->
    Images = [format_image(X) || X <- language:list_images()],
    {jsx:encode(Images), Req, State}.

format_image(Image) ->
    BaseUrl = <<"https://hub.docker.com/r/">>,
    [Name, _] = binary:split(Image, <<":">>),
    #{
        image => Image,
        url => <<BaseUrl/binary, Name/binary, "/">>
    }.
