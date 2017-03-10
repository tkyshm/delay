-module(m).

-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([store/2,
         lookup/2,
         delete/2,
         multi_delete/2,
         find_all_acceptors/0,
         find_all_ready_jobs/0
        ]).

-spec store(atom(), #{}) -> ok.
store(Table, Data) ->
    mnesia:activity(async_dirty, fun mnesia:write/3, [Table, Data, sticky_write], mnesia_frag).

-spec lookup(Table::atom(), Key::term()) -> {ok, term()} | {error, term()}.
lookup(Table, Key) ->
    try mnesia:activity(async_dirty, fun mnesia:read/3, [Table, Key, read], mnesia_frag) of
        Result -> {ok, Result}
    catch
        _TypeOfError:Err -> catch_exception(Err)
    end.

-spec find_all_acceptors() -> {ok, [term()]} | {error, term()}.
find_all_acceptors() ->
    Q = qlc:q([A || A <- mnesia:table(acceptor)]),
    F = fun() -> qlc:eval(Q) end,
    try mnesia:activity(transaction, F, [], mnesia_frag) of
        Result -> {ok, Result}
    catch
        _TypeOfError:Err -> catch_exception(Err)
    end.

-spec delete(Table :: atom(), Key :: binary()) -> ok | transaction_abort.
delete(Table, Key) ->
    mnesia:activity(async_dirty, fun mnesia:delete/3, [Table, Key, sticky_write], mnesia_frag).

-spec multi_delete(Table :: atom(), Key :: binary()) -> ok | transaction_abort.
multi_delete(Table, Keys) ->
    DelF = fun(Key) -> mnesia:delete(Table, Key, sticky_write) end,
    F = fun() -> lists:foreach(DelF, Keys) end,
    mnesia:activity(transaction, F, [], mnesia_frag).

-spec find_all_ready_jobs() -> {ok, [term()]} | {error, term()} .
find_all_ready_jobs() ->
    Q = qlc:q([Job || Job <- mnesia:table(job), Job#job.status == ready]),
    F = fun() -> qlc:eval(Q) end,
    try mnesia:activity(transaction, F, [], mnesia_frag) of
        Result -> {ok, Result}
    catch
        _TypeOfError:Err -> catch_exception(Err)
    end.

-spec catch_exception(Err :: term()) -> {error, term()}.
catch_exception(Err) ->
	case Err of
	    {aborted, {no_exists, _}} ->
		{error, not_found};
	    _ ->
		{error, Err}
	end.
