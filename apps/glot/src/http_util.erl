-module(http_util).

-export([
    decode_body/3,
    success_response/2,
    success_response/3,
    error_response/3,
    set_body_message/2,
    sendfile/4,
    add_cors_headers/1,
    check_design_token/1,
    check_admin_token/1
]).

-define(INVALID_JSON, <<"Invalid json">>).

decode_body(F, Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case jsx:is_json(Body) of
        true ->
            Data = jsx:decode(Body),
            F(Data, Req2, State);
        false ->
            Payload = jsx:encode([{message, ?INVALID_JSON}]),
            {ok, Req3} = cowboy_req:reply(400, [], Payload, Req2),
            {halt, Req3, State}
    end.

success_response(Req, State) ->
    {true, Req, State}.

success_response(Data, Req, State) ->
    Payload = jsx:encode(Data),
    NewReq = cowboy_req:set_resp_body(Payload, Req),
    {true, NewReq, State}.

set_body_message(Msg, Req) ->
    Data = jsx:encode([{message, Msg}]),
    cowboy_req:set_resp_body(Data, Req).

error_response(Msg, Req, State) ->
    Payload = jsx:encode([
        {<<"error">>, Msg}
    ]),
    NewReq = cowboy_req:set_resp_body(Payload, Req),
    {false, NewReq, State}.

sendfile(Fpath, ContentType, Req, State) ->
    Size = filelib:file_size(Fpath),
    NewReq = cowboy_req:set_resp_body_fun(Size, fun(Socket, Transport) ->
        Transport:sendfile(Socket, Fpath)
    end, Req),
    Headers = [{<<"content-type">>, ContentType}],
    {ok, NewReq2} = cowboy_req:reply(200, Headers, NewReq),
    {halt, NewReq2, State}.

add_cors_headers(Req) ->
    Headers = [
        {<<"access-control-allow-methods">>, <<"POST, OPTIONS">>},
        {<<"access-control-allow-origin">>, <<"*">>},
        {<<"access-control-allow-headers">>, <<"Content-Type">>}
    ],
    set_headers(Headers, Req).

set_headers(Headers, Req) ->
    lists:foldl(fun({Name, Value}, R) ->
        cowboy_req:set_resp_header(Name, Value, R)
    end, Req, Headers).

check_design_token(Req) ->
    % Grab token from authorization header
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {_, {<<"token">>, TokenData}, _} ->
            parse_and_validate_design_token(Req, TokenData);
        _ ->
            invalid_header
    end.

check_admin_token(Req) ->
    % Grab token from authorization header
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {_, {<<"token">>, TokenData}, _} ->
            parse_and_validate_admin_token(TokenData);
        _ ->
            invalid_header
    end.

parse_and_validate_design_token(Req, TokenData) ->
    case get_token(<<"design">>, TokenData) of
        {ok, Token} ->
            {Value, _} = cowboy_req:binding(id, Req),
            DesignId = binary_to_integer(Value),
            TokenHash = util:sha1(Token),
            design_model:lookup_design_token(DesignId, TokenHash);
        Invalid ->
            Invalid
    end.

parse_and_validate_admin_token(TokenData) ->
    case get_token(<<"admin">>, TokenData) of
        {ok, Token} ->
            TokenHash = util:sha1(Token),
            design_model:lookup_admin_token(TokenHash);
        Invalid ->
            Invalid
    end.

get_token(Type, TokenData) ->
    case parse_token(TokenData) of
        {ok, Parsed} ->
            case proplists:get_value(<<"type">>, Parsed) of
                Type -> {ok, proplists:get_value(<<"value">>, Parsed, <<"">>)};
                undefined -> wrong_type;
                _ -> wrong_type
            end;
        Invalid -> Invalid
    end.

parse_token(Data) ->
    case binary:split(Data, <<" ">>) of
        [First, Second] ->
            {ok, [parse_token_kv(First), parse_token_kv(Second)]};
        _ -> no_parse
    end.

parse_token_kv(Data) ->
    case binary:split(util:trim(Data), <<"=">>) of
        [Key, QuotedValue] -> {Key, util:unquote(QuotedValue)};
        _ -> no_parse
    end.
