-module(delay).

-export([send_reciever/1]).

-spec send_reciever(Pid::pid()) -> boolean().
send_reciever(Pid) -> erlang:send_nosuspend(Pid, dequeue).
