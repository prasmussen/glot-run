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
    save/1,
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
    {ok, Tokens} = persist:load(Fname, sets:new()),
    {ok, #state{filename=Fname, tokens=Tokens}}.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call({list}, _, State=#state{tokens=Tokens}) ->
    {reply, Tokens, State};
handle_call({save, Token}, _, State=#state{tokens=Tokens, filename=Fname}) ->
    NewTokens = sets:add_element(Token, Tokens),
    ok = persist:save(Fname, NewTokens),
    {reply, ok, State#state{tokens=NewTokens}};
handle_call({delete, Token}, _, State=#state{tokens=Tokens, filename=Fname}) ->
    NewTokens = sets:del_element(Token, Tokens),
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

save(Token) ->
    gen_server:call(?MODULE, {save, Token}).

delete(Token) ->
    gen_server:call(?MODULE, {delete, Token}).
