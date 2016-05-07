-module(glot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    MaxRestart = 600,
    MaxTime = 60,

    Children = [
        ?CHILD(docker_attach_sup, supervisor),
        ?CHILD(http_log_srv, worker),
        ?CHILD(event_log_srv, worker),
        ?CHILD(language_srv, worker),
        ?CHILD(user_srv, worker)
    ],

    {ok, {{one_for_one, MaxRestart, MaxTime}, Children}}.
