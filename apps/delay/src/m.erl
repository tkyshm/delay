-module(m).

-export([store/2, lookup/2]).

-spec store(atom(), #{}) -> ok.
store(Table, Data) ->
    F = fun() -> mnesia:write(Table, Data, sticky_write) end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

-spec lookup(Table::atom(), Key::term()) -> {ok, term()} | {not_found, term()}.
lookup(Table, Key) ->
    F = fun() -> mnesia:read(Table, Key, read) end,
    try mnesia:activity(async_dirty, F, [], mnesia_frag) of
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
