-module(http_auth).

-export([
    authorize_admin/2,
    authorize_user/2
]).

-define(WWW_AUTHENTICATE, <<"Token <token>">>).
-define(MISSING_TOKEN, <<"Missing auth token">>).
-define(WRONG_TOKEN, <<"Wrong auth token">>).

authorize_admin(Req, State) ->
    authorize(Req, State, fun admin_token:is_valid/1).

authorize_user(Req, State) ->
    authorize(Req, State, fun user_token:is_valid/1).

authorize(Req, State, ValidateFn) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {ok, {<<"token">>, Token}, _} ->
            validate_token(Req, State, ValidateFn, Token);
        _ ->
            Data = jsx:encode(#{message => ?MISSING_TOKEN}),
            Req2 = cowboy_req:set_resp_body(Data, Req),
            {{false, ?WWW_AUTHENTICATE}, Req2, State}
    end.

validate_token(Req, State, ValidateFn, Token) ->
    case ValidateFn(Token) of
        true ->
            {true, Req, State};
        false ->
            Data = jsx:encode(#{message => ?WRONG_TOKEN}),
            Req2 = cowboy_req:set_resp_body(Data, Req),
            {{false, ?WWW_AUTHENTICATE}, Req2, State}
    end.
