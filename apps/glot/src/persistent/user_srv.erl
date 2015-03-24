-module(user_srv).
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

    get/1,
    list/0,
    save/1,
    delete/1,
    get_by_token/1
]).

-record(state, {
    filename,
    users
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Fname = config:users_data_path(),
    {ok, Users} = persist:load(Fname, maps:new()),
    {ok, #state{filename=Fname, users=Users}}.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call({get, Id}, _, State=#state{users=Users}) ->
    Res = case maps:find(Id, Users) of
        {ok, User} -> {ok, User};
        error -> {error, not_found}
    end,
    {reply, Res, State};
handle_call({get_by_token, Token}, _, State=#state{users=Users}) ->
    Found = maps:fold(fun(_, User, Acc) ->
        case Token == maps:get(token, User) of
            true -> [User|Acc];
            false -> Acc
        end
    end, [], Users),
    {reply, Found, State};
handle_call({list}, _, State=#state{users=Users}) ->
    {reply, maps:values(Users), State};
handle_call({save, Id, User}, _, State=#state{users=Users, filename=Fname}) ->
    NewUsers = maps:put(Id, User, Users),
    ok = persist:save(Fname, NewUsers),
    {reply, User, State#state{users=NewUsers}};
handle_call({delete, Id}, _, State=#state{users=Users, filename=Fname}) ->
    NewUsers = maps:remove(Id, Users),
    ok = persist:save(Fname, NewUsers),
    {reply, ok, State#state{users=NewUsers}};
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

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

get_by_token(Token) ->
    gen_server:call(?MODULE, {get_by_token, Token}).

list() ->
    gen_server:call(?MODULE, {list}).

save(User) ->
    Id = maps:get(id, User),
    gen_server:call(?MODULE, {save, Id, User}).

delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).
