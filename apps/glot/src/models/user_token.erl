-module(user_token).

-export([
    get/1,
    list/0,
    save/1,
    delete/1,
    exists/1,
    is_valid/1
]).

identifier(Token) ->
    util:sha1(Token).

get(Id) ->
    {ok, Token} = maps:find(Id, token_srv:list()),
    {Id, Token}.

list() ->
    maps:to_list(token_srv:list()).

save(Token) ->
    Id = identifier(Token),
    token_srv:save(Id, Token).

delete(Id) ->
    token_srv:delete(Id).

exists(Id) ->
    maps:is_key(Id, token_srv:list()).

is_valid(Token) ->
    Id = identifier(Token),
    maps:is_key(Id, token_srv:list()).
