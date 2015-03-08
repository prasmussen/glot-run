-module(token_srv).
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
    tokens
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Fname = config:tokens_data_path(),
    {ok, Tokens} = persist:load(Fname, maps:new()),
    {ok, #state{filename=Fname, tokens=Tokens}}.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call({list}, _, State=#state{tokens=Tokens}) ->
    {reply, Tokens, State};
handle_call({save, Id, Token}, _, State=#state{tokens=Tokens, filename=Fname}) ->
    NewTokens = maps:put(Id, Token, Tokens),
    ok = persist:save(Fname, NewTokens),
    {reply, Id, State#state{tokens=NewTokens}};
handle_call({delete, Id}, _, State=#state{tokens=Tokens, filename=Fname}) ->
    NewTokens = maps:remove(Id, Tokens),
    ok = persist:save(Fname, NewTokens),
    {reply, ok, State#state{tokens=NewTokens}};
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

save(Id, Token) ->
    gen_server:call(?MODULE, {save, Id, Token}).

delete(Token) ->
    gen_server:call(?MODULE, {delete, Token}).
