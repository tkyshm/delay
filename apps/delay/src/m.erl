-module(m).

-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([store/2,
         lookup/2,
         delete/2,
         find_all_recievers/0,
         find_all_ready_jobs/0
        ]).

-spec store(atom(), #{}) -> ok.
store(Table, Data) ->
    mnesia:activity(async_dirty, fun mnesia:write/3, [Table, Data, sticky_write], mnesia_frag).

-spec lookup(Table::atom(), Key::term()) -> {ok, term()} | {not_found, term()}.
lookup(Table, Key) ->
    try mnesia:activity(async_dirty, fun mnesia:read/3, [Table, Key, read], mnesia_frag) of
        Result -> {ok, Result}
    catch
        _TypeOfError:Err ->
            case Err of
                {aborted, {no_exists, _}} ->
                    {error, not_found};
                _ ->
                    {error, Err}
            end
    end.

-spec find_all_recievers() -> {aborted, Reason :: term()} | {atomic, Result :: term()}.
find_all_recievers() ->
    Q = qlc:q([R || R <- mnesia:table(reciever)]),
    F = fun() -> qlc:eval(Q) end,
    try mnesia:activity(transaction, F, [], mnesia_frag) of
        Result -> {ok, Result}
    catch
        _TypeOfError:Err -> catch_exception(Err)
    end.

-spec delete(Table :: atom(), Key :: binary()) -> ok | transaction_abort.
delete(Table, Key) ->
    mnesia:activity(async_dirty, fun mnesia:delete/1, [{Table, Key}], mnesia_frag).

-spec find_all_ready_jobs() -> {aborted, Reason :: term()} | {atomic, Result :: term()}.
find_all_ready_jobs() ->
    Q = qlc:q([Job || Job <- mnesia:table(job), Job#job.status == ready]),
    F = fun() -> qlc:eval(Q) end,
    try mnesia:activity(transaction, F, [], mnesia_frag) of
        Result -> {ok, Result}
    catch
        _TypeOfError:Err -> catch_exception(Err)
    end.

-spec catch_exception(Err::term()) -> {aborted, Reason :: term()} | {atomic, Result :: term()}.
catch_exception(Err) ->
    case Err of
        {aborted, {no_exists, _}} ->
            {error, not_found};
        _ ->
            {error, Err}
    end.
