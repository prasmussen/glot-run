-module(log).
-export([
    event/1
]).

event(Event) when is_list(Event) ->
    event(list_to_binary(Event));
event(Event) ->
    log_srv:log_event(#{event => Event}).
