-module(language).

-export([
    get/1,
    exists/1,
    save/3,
    delete/1,
    list/0,
    list_names/0,
    list_images/0,
    list_versions/1,
    get_image/2,
    is_supported/1,
    is_supported/2
]).

identifier(Name, Version) ->
    util:sha1(<<Name/binary, Version/binary>>).

save(Name, Version, Image) ->
    Id = identifier(Name, Version),
    language_srv:save(Id, {Name, Version, Image}).

delete(Id) ->
    language_srv:delete(Id).

get(Id) ->
    {ok, {Name, Vsn, Image}} = maps:find(Id, language_srv:list()),
    {Id, Name, Vsn, Image}.

exists(Id) ->
    maps:is_key(Id, language_srv:list()).

is_supported(Name) ->
    lists:member(Name, list_names()).

is_supported(Name, Version) ->
    Id = identifier(Name, Version),
    maps:is_key(Id, language_srv:list()).

list() ->
    maps:fold(fun(Id, {Name, Vsn, Image}, Acc) ->
        [{Id, Name, Vsn, Image}|Acc]
    end, [], language_srv:list()).

list_names() ->
    Names = maps:fold(fun(_, {Name, _, _}, Acc) ->
        [Name|Acc]
    end, [], language_srv:list()),
    sort_and_remove_duplicates(Names).

list_images() ->
    Images = maps:fold(fun(_, {_, _, Image}, Acc) ->
        [Image|Acc]
    end, [], language_srv:list()),
    sort_and_remove_duplicates(Images).

list_versions(Name) ->
    Versions = maps:fold(fun(_, {LangName, Vsn, _}, Acc) ->
        case LangName =:= Name of
            true -> [Vsn|Acc];
            false -> Acc
        end
    end, [], language_srv:list()),
    sort_and_remove_duplicates(Versions).

get_image(Name, Version) ->
    Id = identifier(Name, Version),
    {ok, {_, _, Image}} = maps:find(Id, language_srv:list()),
    Image.

sort_and_remove_duplicates(List) ->
    Set = gb_sets:from_list(List),
    gb_sets:to_list(Set).
