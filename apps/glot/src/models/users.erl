-module(users).
-export([
    get/1,
    list/0,
    save/1,
    update/2,
    delete/1,
    valid_token/1
]).

identifier() ->
    Uuid = uuid:uuid_to_string(uuid:get_v4()),
    list_to_binary(Uuid).

valid_token(Token) ->
    case user_srv:get_by_token(Token) of
        [] -> false;
        [_] -> true
    end.

get(Id) ->
    user_srv:get(Id).

list() ->
    user_srv:list().

save(Data) ->
    User = prepare_save(Data),
    user_srv:save(User).

update(OldUser, NewUser) ->
    User = prepare_update(OldUser, NewUser),
    user_srv:save(User).

delete(Id) ->
    user_srv:delete(Id).

prepare_save(Data) ->
    Now = iso8601:format(os:timestamp()),
    Data#{
        id => identifier(),
        created => Now,
        modified => Now
    }.

prepare_update(OldUser, NewUser) ->
    User = maps:merge(OldUser, NewUser),
    Now = iso8601:format(os:timestamp()),
    User#{
        modified := Now
    }.
