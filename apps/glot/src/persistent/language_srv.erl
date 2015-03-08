-module(language_srv).
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

    list/0,
    save/2,
    delete/1
]).

-record(state, {
    filename,
    languages
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Fname = config:languages_data_path(),
    {ok, Langs} = persist:load(Fname, maps:new()),
    {ok, #state{filename=Fname, languages=Langs}}.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call({list}, _, State=#state{languages=Langs}) ->
    {reply, Langs, State};
handle_call({save, Id, Language}, _, State=#state{languages=Langs, filename=Fname}) ->
    NewLangs = maps:put(Id, Language, Langs),
    ok = persist:save(Fname, NewLangs),
    {reply, Id, State#state{languages=NewLangs}};
handle_call({delete, Id}, _, State=#state{languages=Langs, filename=Fname}) ->
    NewLangs = maps:remove(Id, Langs),
    ok = persist:save(Fname, NewLangs),
    {reply, ok, State#state{languages=NewLangs}};
handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsc, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    Reason.

list() ->
    gen_server:call(?MODULE, {list}).

save(Id, Language) ->
    gen_server:call(?MODULE, {save, Id, Language}).

delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).
