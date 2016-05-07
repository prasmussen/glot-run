-module(docker_attach_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1,

    start_child/0
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MaxRestart = 1200,
    MaxTime = 60,
    Spec = {
        docker_attach,
        {docker_attach, start_link, []},
        temporary, 1000, worker, [docker_attach]
    },

    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [Spec]}}.

start_child() ->
    supervisor:start_child(?MODULE, []).
