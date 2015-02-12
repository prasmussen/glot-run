-module(token).

-export([
    save/1,
    delete/1,
    is_valid/1
]).

save(Token) ->
    datastore:token_save(Token).

delete(Token) ->
    datastore:token_delete(Token).

is_valid(Token) ->
    Tokens = datastore:token_list(),
    sets:is_element(Token, Tokens).
