-module(datastore_db).

-export([
    open/1,
    close/1,
    language_list/1,
    language_save/2,
    language_delete/2,
    token_list/1,
    token_save/2,
    token_delete/2
]).

-define(TOKENS_KEY, <<"tokens">>).
-define(LANGUAGES_KEY, <<"languages">>).

open_options() ->
    [
        {create_if_missing, true},
        {compression, true},
        {verify_compactions, true},
        {use_bloomfilter, true},
        {sst_block_size, 4096},
        {block_restart_interval, 16}
    ].

read_options() ->
    [{verify_checksums, true}].

write_options() ->
    [{sync, true}].

open(Path) ->
    eleveldb:open(Path, open_options()).

close(Db) ->
    eleveldb:close(Db).

language_save(Db, {Id, Language}) ->
    Actions = [
        add_language_action(Db, Id, Language)
    ],
    ok = eleveldb:write(Db, Actions, write_options()),
    Language.

language_delete(Db, {Id}) ->
    Actions = [
        remove_language_action(Db, Id)
    ],
    ok = eleveldb:write(Db, Actions, write_options()),
    Id.

token_save(Db, {Token}) ->
    Actions = [
        add_token_action(Db, Token)
    ],
    ok = eleveldb:write(Db, Actions, write_options()),
    Token.

token_delete(Db, {Token}) ->
    Actions = [
        remove_token_action(Db, Token)
    ],
    ok = eleveldb:write(Db, Actions, write_options()),
    Token.

language_list(Db) ->
    Res = eleveldb:get(Db, ?LANGUAGES_KEY, read_options()),
    unmarshal(Res, maps:new()).

add_language_action(Db, Id, Language) ->
    Languages = maps:put(Id, Language, language_list(Db)),
    {put, ?LANGUAGES_KEY, marshal(Languages)}.

remove_language_action(Db, Id) ->
    Languages = maps:remove(Id, language_list(Db)),
    {put, ?LANGUAGES_KEY, marshal(Languages)}.

token_list(Db) ->
    Res = eleveldb:get(Db, ?TOKENS_KEY, read_options()),
    unmarshal(Res, sets:new()).

add_token_action(Db, Token) ->
    Tokens = sets:add_element(Token, token_list(Db)),
    {put, ?TOKENS_KEY, marshal(Tokens)}.

remove_token_action(Db, Token) ->
    Tokens = sets:del_element(Token, token_list(Db)),
    {put, ?TOKENS_KEY, marshal(Tokens)}.

marshal(Term) ->
    term_to_binary(Term).

unmarshal({ok, Binary}, _) -> binary_to_term(Binary);
unmarshal(not_found, Default) -> Default.
