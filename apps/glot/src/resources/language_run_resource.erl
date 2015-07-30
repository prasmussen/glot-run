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

-define(MISSING_FILES, <<"Missing files">>).
-define(INCOMPLETE_FILE, <<"One or more files are incomplete">>).
-define(TIMEOUT_ERROR, <<"Code exceeded the maximum allowed running time">>).
-define(MAX_OUTPUT_SIZE_ERROR, <<"Output exceeded the maximum allowed size">>).

-record(state, {
    name,
    version
}).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    Methods = [<<"POST">>],
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
    {Name, _} = cowboy_req:binding(name, Req),
    {Vsn, _} = cowboy_req:binding(version, Req),

    case language:is_supported(Name, Vsn) of
        true ->
            {true, Req, State#state{name=Name, version=Vsn}};
        false ->
            {false, Req, State}
    end.

accept_post(Req, State) ->
    http_util:decode_body(fun validate_and_run_code/3, Req, State).

validate_and_run_code(Data, Req, State) ->
    case validate_files(Data) of
        ok ->
            run_code(Data, Req, State);
        {error, missing_files} ->
            Res = jsx:encode(#{<<"message">> => ?MISSING_FILES}),
            {false, cowboy_req:set_resp_body(Res, Req), State};
        {error, incomplete_file} ->
            Res = jsx:encode(#{<<"message">> => ?INCOMPLETE_FILE}),
            {false, cowboy_req:set_resp_body(Res, Req), State}
    end.

validate_files(Data) ->
    case proplists:get_value(<<"files">>, Data, []) of
        [] -> {error, missing_files};
        Files -> ensure_complete_files(Files)
    end.

ensure_complete_files(Files) ->
    Incomplete = lists:any(fun(File) ->
        Name = proplists:get_value(<<"name">>, File, missing),
        Content = proplists:get_value(<<"content">>, File, missing),
        Name =:= missing orelse Content =:= missing
    end, Files),
    case Incomplete of
        true -> {error, incomplete_file};
        false -> ok
    end.

run_code(Data, Req, State=#state{name=Name, version=Vsn}) ->
    case language_run:run(Name, Vsn, Data) of
        {ok, Res} ->
            {true, cowboy_req:set_resp_body(Res, Req), State};
        {error, timeout} ->
            Res = jsx:encode(#{<<"message">> => ?TIMEOUT_ERROR}),
            {false, cowboy_req:set_resp_body(Res, Req), State};
        {error, max_output_size} ->
            Res = jsx:encode(#{<<"message">> => ?MAX_OUTPUT_SIZE_ERROR}),
            {false, cowboy_req:set_resp_body(Res, Req), State};
        {error, Msg} ->
            Res = jsx:encode(#{<<"message">> => Msg}),
            {false, cowboy_req:set_resp_body(Res, Req), State}
    end.
