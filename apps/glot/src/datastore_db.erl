-module(datastore_db).

-export([
    open/1,
    close/1,
    token_list/1,
    token_save/2,
    token_delete/2
]).

-define(TOKENS_KEY, <<"tokens">>).

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
