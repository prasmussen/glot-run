-module(util).

-export([
    sha1/1,
    pid_to_binary/1
]).

sha1(Data) ->
    hexstring(crypto:hash(sha, Data)).

hexstring(<<X:128/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:224/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~56.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:384/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~96.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~128.16.0b", [X])).

pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).
