-module(log_srv).
-behaviour(gen_server).
-export([
    start_link/0,
    stop/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2,

    log_http/1
]).

-record(state, {
    http
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Fname = config:http_log_path(),
    {ok, Http} = file:open(Fname, [append, delayed_write, {encoding, utf8}]),
    {ok, #state{http=Http}}.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call(_event, _From, State) ->
    {noreply, State}.

handle_cast({http, Data}, State=#state{http=Http}) ->
    log_write(Http, Data),
    {noreply, State};
handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsc, State, _Extra) ->
    {ok, State}.

terminate(Reason, #state{http=Http}) ->
    file:close(Http),
    Reason.

pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

log_write(File, Data) ->
    Json = jsx:encode(Data),
    file:write(File, <<Json/binary, "\n">>).

log_http(Data) ->
    Data2 = Data#{
        timestamp => iso8601:format(now()),
        pid => pid_to_binary(self())
    },
    gen_server:cast(?MODULE, {http, Data2}).
