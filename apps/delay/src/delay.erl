-module(delay).

-export([send_acceptor/1]).

-spec send_acceptor(Pid::pid()) -> boolean().
send_acceptor(Pid) -> erlang:send_nosuspend(Pid, dequeue).
