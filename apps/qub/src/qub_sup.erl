%%%-------------------------------------------------------------------
%% @doc qub top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(qub_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    JobChild = {'job_sup', {'job_sup', start_link, []},
                Restart, Shutdown, Type, ['job_sup']},

    {ok, {SupFlags, [JobChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
