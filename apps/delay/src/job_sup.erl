%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2017, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2017-03-03 23:44:22.186529
%%%-------------------------------------------------------------------
-module(job_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         spawn_child/3
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc
%% Spawn job worker
%%
%% @spec spawn_child(Input, DelayTime) -> supervisor:startchild_ret().
%% @end
-spec spawn_child(binary(), binary(), non_neg_integer()) -> supervisor:startchild_ret().
spawn_child(Event, Hook, DelayTime) ->
    Uid = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
    {_, Pid} = supervisor:start_child(?SERVER, [Uid, Event, Hook, DelayTime]),
    gen_fsm:send_event(Pid, {execute, Event}),
    Uid.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {'job_worker', {'job_worker', start_link, []},
              Restart, Shutdown, Type, ['job_worker']},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

