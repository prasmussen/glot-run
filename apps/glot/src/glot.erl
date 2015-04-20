-module(glot).
-export([
    start/0,
    start_link/0,
    stop/0,
    priv_dir/0
]).

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
    [ensure_started(App) || App <- applications()],
    lager:info("start_link glot"),
    glot_sup:start_link().

%% @spec start() -> ok
start() ->
    [ensure_started(App) || App <- applications()],
    lager:info("start glot"),
    application:start(glot).

%% @spec stop() -> ok
stop() ->
    lager:info("stop glot"),
    Res = application:stop(glot),
    [application:stop(App) || App <- lists:reverse(applications())],
    Res.

applications() ->
    [
        syntax_tools,
        compiler,
        goldrush,
        lager,
        crypto,
        ranch,
        cowlib,
        cowboy,
        jsx,
        asn1,
        public_key,
        ssl,
        idna,
        hackney,
        iso8601,
        quickrand,
        uuid
    ].

priv_dir() ->
    {ok, App} = application:get_application(?MODULE),
    case code:priv_dir(App) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(App)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
