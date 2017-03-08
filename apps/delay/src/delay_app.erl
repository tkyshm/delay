%%%-------------------------------------------------------------------
%% @doc delay public API
%% @end
%%%-------------------------------------------------------------------

-module(delay_app).

-behaviour(application).

%% Application callbacks
-export([start/2, profile_output/0, stop/1]).

-include("schema.hrl").

-define(DEFAULT_PORT, 9020).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_profile(),

    %% init mnesia (TODO: escripts)
    case create_table(job, 128) of
        {aborted, Reason1} ->
            error_logger:error_msg("failed to create tables: reason=~p", [Reason1]);
        _ ->
            ok
    end,

    case create_table(reciever, 128) of
        {aborted, Reason2} ->
            error_logger:error_msg("failed to create tables: reason=~p", [Reason2]);
        _ ->
            ok
    end,

    case application:get_env(port) of
        {ok, Port} -> start_api_server(Port);
        _ -> start_api_server(?DEFAULT_PORT)
    end,
    delay_sup:start_link().

profile_output() ->
    eprof:stop_profiling(),
    eprof:log("epms_procs.profile"),
    eprof:analyze(procs),
    eprof:log("epms_total.profile"),
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
    {ok, _} = cowboy:start_clear(delay_httpd, 100, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }).

%% TODO: for replications
-spec create_table(atom(), non_neg_integer()) -> {aborted, Reason::term()} | {ok, already_exists} | {ok, created}.
create_table(Table, Frag) ->
   case mnesia:create_table(Table, [{type, set}, {frag_properties,
                               [{node_pool, [node()]},
                                {n_fragments, Frag}
                               ]},
                              {attributes, record_info(fields, job)}]) of
       {aborted, {already_exists, job}} ->
           {ok, already_exists};
       {aborted, Reason} ->
           {aborted, Reason};
       _ ->
           {ok, created}
   end.
