-module(persist).
-export([
    load/2,
    save/2
]).


load(Fname, Default) ->
    case file:read_file(Fname) of
        {ok, Binary} -> {ok, binary_to_term(Binary)};
        {error, enoent} -> {ok, Default};
        Error -> Error
    end.

save(Dst, Data) ->
    Src = tmp_fname(Dst),
    case file:write_file(Src, term_to_binary(Data)) of
        ok -> file:rename(Src, Dst);
        Error -> Error
    end.

tmp_fname(Path) ->
    filename:join(
        filename:dirname(Path),
        "." ++ filename:basename(Path) ++ ".tmp"
    ).
