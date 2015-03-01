-module(log).
-export([
    http/1,
    event/1,
    write/2
]).

event(Event) when is_list(Event) ->
    event(list_to_binary(Event));
event(Event) ->
    event_log_srv:append(#{event => Event}).

http(Http) ->
    http_log_srv:append(Http).

write(File, Data) ->
    Json = jsx:encode(Data),
    file:write(File, <<Json/binary, "\n">>).
