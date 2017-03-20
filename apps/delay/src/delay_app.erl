%%%-------------------------------------------------------------------
%% @doc delay public API
%% @end
%%%-------------------------------------------------------------------

-module(delay_app).

-behaviour(application).

%% Application callbacks
-export([start/2, profile_output/0, stop/1]).

-include("delay.hrl").
-include("schema.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_profile(),

    %% init mnesia (TODO: escripts)
    %%case create_job_table(6) of
    %%    {aborted, Reason1} ->
    %%        error_logger:error_msg("failed to create tables: reason=~p", [Reason1]);
    %%    _ ->
    %%        ok
    %%end,

    %%case create_acceptor_table(6) of
    %%    {aborted, Reason2} ->
    %%        error_logger:error_msg("failed to create tables: reason=~p", [Reason2]);
    %%    _ ->
    %%        ok
    %%end,

    %% initialize hackney pool for webhook
    Options = [{timeout, ?WEBHOOK_TIMEOUT}, {max_connections, ?WEBHOOK_MAX_CONNECTIONS}],
    ok = hackney_pool:start_pool(webhook, Options),

    case application:get_env(port) of
        {ok, Port} -> start_api_server(Port);
        _          -> start_api_server(?DEFAULT_PORT)
    end,
    delay_sup:start_link().

profile_output() ->
    eprof:stop_profiling(),
    eprof:log("delay_procs.profile"),
    eprof:analyze(procs),
    eprof:log("delay_total.profile"),
    eprof:analyze(total).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_profile() ->
    case application:get_env(profile) of
        {ok, true} ->
            {ok, _Pid} = eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.

start_api_server(Port) ->
    Dispatch = cowboy_router:compile([
        { '_', [
            { "/api/enqueue", enqueue_handler, [] },
            { "/api/dequeue", dequeue_handler, [] }
        ]}
    ]),
    Opts = #{
      env             => #{dispatch => Dispatch},
      request_timeout => ?MAX_POLLING_TIMEOUT * 1000
     },
    {ok, _} = cowboy:start_clear(delay_httpd, 100, [{port, Port}], Opts).

%% TODO: for replications
-spec create_job_table(non_neg_integer()) -> {aborted, Reason::term()} | {ok, already_exists} | {ok, created}.
create_job_table(Frag) ->
    TabDef = [{type, set},
              {disc_copies, [node()]},
              {frag_properties,
               [{node_pool, [node()]},
                {n_fragments, Frag}
               ]},
              {attributes, record_info(fields, job)}],
    case mnesia:create_table(job, TabDef) of
        {aborted, {already_exists, job}} ->
            {ok, already_exists};
        {aborted, Reason} ->
            {aborted, Reason};
        _ ->
            {ok, created}
    end.

-spec create_acceptor_table(non_neg_integer()) -> {aborted, Reason::term()} | {ok, already_exists} | {ok, created}.
create_acceptor_table(Frag) ->
    TabDef = [{type, set},
              {disc_copies, [node()]},
              {frag_properties,
               [{node_pool, [node()]},
                {n_fragments, Frag}
               ]},
              {attributes, record_info(fields, acceptor)}],
    case mnesia:create_table(acceptor, TabDef) of
        {aborted, {already_exists, acceptor}} ->
            {ok, already_exists};
        {aborted, Reason} ->
            {aborted, Reason};
        _ ->
            {ok, created}
    end.
