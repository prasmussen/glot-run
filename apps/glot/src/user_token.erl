-module(user_token).

-export([
    list/0,
    save/1,
    delete/1,
    is_valid/1
]).


list() ->
    sets:to_list(token_srv:list()).

save(Token) ->
    token_srv:save(Token).

delete(Token) ->
    token_srv:delete(Token).

is_valid(Token) ->
    sets:is_element(Token, token_srv:list()).
