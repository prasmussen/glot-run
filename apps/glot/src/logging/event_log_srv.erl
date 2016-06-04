-module(event_log_srv).
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

    append/1
]).

-record(state, {
    file
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Path = config:event_log_path(),
    {ok, File} = file:open(Path, [append, delayed_write, {encoding, utf8}]),
    {ok, #state{file=File}}.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast({append, Data}, State=#state{file=File}) ->
    log:write(File, Data),
    {noreply, State};
handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsc, State, _Extra) ->
    {ok, State}.

terminate(Reason, #state{file=File}) ->
    file:close(File),
    Reason.

append(Data) ->
    Data2 = Data#{
        timestamp => iso8601:format(os:timestamp()),
        pid => util:pid_to_binary(self())
    },
    gen_server:cast(?MODULE, {append, Data2}).
