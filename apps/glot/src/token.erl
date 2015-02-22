-module(token).

-export([
    list/0,
    save/1,
    delete/1,
    is_valid_user/1,
    is_valid_admin/1
]).


list() ->
    sets:to_list(datastore:token_list()).

save(Token) ->
    datastore:token_save(Token).

delete(Token) ->
    datastore:token_delete(Token).

is_valid_user(Token) ->
    Tokens = datastore:token_list(),
    sets:is_element(Token, Tokens).

is_valid_admin(Token) ->
    config:admin_token() =:= Token.
