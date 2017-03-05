-module(m).

-export([store/2, lookup/2]).

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
