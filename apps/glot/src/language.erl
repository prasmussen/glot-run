-module(language).

-export([
    get/1,
    exists/1,
    save/3,
    delete/1,
    list/0,
    list_languages/0,
    list_versions/1,
    get_image/2,
    is_supported/1,
    is_supported/2
]).

identifier(Language, Version) ->
    util:sha1(<<Language/binary, Version/binary>>).

save(Language, Version, Image) ->
    Id = identifier(Language, Version),
    datastore:language_save(Id, {Language, Version, Image}).

delete(Id) ->
    datastore:language_delete(Id).

get(Id) ->
    {ok, {Lang, Vsn, Image}} = maps:find(Id, datastore:language_list()),
    {Id, Lang, Vsn, Image}.

exists(Id) ->
    maps:is_key(Id, datastore:language_list()).

is_supported(Language) ->
    lists:member(Language, list()).

is_supported(Language, Version) ->
    maps:is_key({Language, Version}, datastore:language_list()).

list() ->
    maps:fold(fun(Id, {Lang, Vsn, Image}, Acc) ->
        [{Id, Lang, Vsn, Image}|Acc]
    end, [], datastore:language_list()).

list_languages() ->
    Languages = maps:fold(fun(_, {Lang, _, _}, Acc) ->
        [Lang|Acc]
    end, [], datastore:language_list()),
    sort_and_remove_duplicates(Languages).

list_versions(Language) ->
    Versions = maps:fold(fun(_, {Lang, Vsn, _}, Acc) ->
        case Lang =:= Language of
            true -> [Vsn|Acc];
            false -> Acc
        end
    end, [], datastore:language_list()),
    sort_and_remove_duplicates(Versions).

get_image(Language, Version) ->
    Id = identifier(Language, Version),
    {ok, Image} = maps:find(Id, datastore:language_list()),
    Image.

sort_and_remove_duplicates(List) ->
    Set = gb_sets:from_list(List),
    gb_sets:to_list(Set).
