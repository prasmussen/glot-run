-module(datastore).
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

    language_list/0,
    language_save/2,
    language_delete/1,

    token_list/0,
    token_save/1,
    token_delete/1
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    lager:info("datastore init"),
    case datastore_db:open(config:datastore_path()) of
        {ok, Db} -> {ok, Db};
        {error, Reason} ->
            lager:error("Failed to open database: ~p", [Reason]),
            {stop, Reason}
    end.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call({language_list}, _From, Db) ->
    {reply, datastore_db:language_list(Db), Db};
handle_call({language_save, Args}, _From, Db) ->
    {reply, datastore_db:language_save(Db, Args), Db};
handle_call({language_delete, Args}, _From, Db) ->
    {reply, datastore_db:language_delete(Db, Args), Db};
handle_call({token_list}, _From, Db) ->
    {reply, datastore_db:token_list(Db), Db};
handle_call({token_save, Args}, _From, Db) ->
    {reply, datastore_db:token_save(Db, Args), Db};
handle_call({token_delete, Args}, _From, Db) ->
    {reply, datastore_db:token_delete(Db, Args), Db}.

handle_cast(_Event, Db) ->
    {noreply, Db}.

handle_info(_Event, Db) ->
    {noreply, Db}.

code_change(_OldVsc, Db, _Extra) ->
    {ok, Db}.

terminate(_Reason, Db) ->
    lager:info("closing db"),
    datastore_db:close(Db).

language_list() ->
    gen_server:call(?MODULE, {language_list}).

language_save(Id, Language) ->
    Args = {Id, Language},
    gen_server:call(?MODULE, {language_save, Args}).

language_delete(Id) ->
    Args = {Id},
    gen_server:call(?MODULE, {language_delete, Args}).

token_list() ->
    gen_server:call(?MODULE, {token_list}).

token_save(Token) ->
    Args = {Token},
    gen_server:call(?MODULE, {token_save, Args}).

token_delete(Token) ->
    Args = {Token},
    gen_server:call(?MODULE, {token_delete, Args}).
