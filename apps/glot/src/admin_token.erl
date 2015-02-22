-module(admin_token).

-export([
    is_valid/1
]).

is_valid(Token) ->
    config:admin_token() =:= Token.
