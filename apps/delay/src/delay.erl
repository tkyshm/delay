-module(delay).

-export([send_acceptor/1,
         create_job_table/1,
         create_acceptor_table/1,
         create_schema/0,
         add_replication/0
        ]).

-include("delay.hrl").
-include("schema.hrl").

-spec send_acceptor(Pid::pid()) -> boolean().
send_acceptor(Pid) -> erlang:send_nosuspend(Pid, dequeue).

-spec create_schema() -> ok.
create_schema() ->
    %% init mnesia (TODO: escripts)
    case create_job_table(6) of
        {aborted, Reason1} ->
            error_logger:error_msg("failed to create tables: reason=~p", [Reason1]);
        _ ->
            ok
    end,

    case create_acceptor_table(6) of
        {aborted, Reason2} ->
            error_logger:error_msg("failed to create tables: reason=~p", [Reason2]);
        _ ->
            ok
    end,

    ok.

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

add_replication() ->
    mnesia:change_config(extra_db_nodes, nodes()),
    mnesia:add_table_copy(job, node(), disc_copies),
    mnesia:add_table_copy(acceptor, node(), disc_copies),
    ok.

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
