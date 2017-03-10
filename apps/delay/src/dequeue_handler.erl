-module(dequeue_handler).

-export([init/2]).
-export([info/3]).
-export([dequeue/0]).
-export([terminate/3]).

-include("schema.hrl").
-include("delay.hrl").

init(Req, State) ->
    Timeout = cowboy_req:header(<<"x-delay-timeout">>, Req, ?DEFAULT_POLLING_TIMEOUT) * 1000,
    MaxTimeout = ?MAX_POLLING_TIMEOUT * 1000,
    Ua = cowboy_req:header(<<"user-agent">>, Req, <<"">>),
    {Ip, Port} = cowboy_req:peer(Req),
    m:store(acceptor, #acceptor{pid = self(),
                                node = node(),
                                ip = Ip,
                                port = Port,
                                ua = Ua,
                                created_at = erlang:system_time(seconds)
                               }),
    if
        Timeout < MaxTimeout -> erlang:send_after(Timeout, self(), timeout);
        true                 -> erlang:send_after(MaxTimeout, self(), timeout)
    end,
    {cowboy_loop, Req, State, hibernate}.

info(timeout, Req, State) ->
    {stop, Req, State};
info(dequeue, Req, State) ->
    case dequeue() of
        {ok, not_found} ->
            {stop, Req, State};
        {ok, Resp} ->
            {stop, cowboy_req:reply(200, #{}, Resp, Req), State};
        {error, ErrResp} ->
            {stop, cowboy_req:reply(500, #{}, ErrResp, Req), State}
    end;
info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
    m:delete(acceptor, self()),
    ok.

dequeue() ->
    case m:find_all_ready_jobs() of
        {ok, []} ->
            {ok, not_found};
        {ok, Jobs} ->
            lists:foreach(fun(J) ->
                gen_fsm:send_event(J#job.pid, dequeue)
            end, Jobs),
            JobJson = lists:map(fun(J) ->
                #{<<"data">> => J#job.data, <<"uid">> => J#job.uid}
            end, Jobs),
            Resp = jiffy:encode(JobJson),
            {ok, Resp};
        _ ->
            Resp = <<"{\"error\":\"Internal Server Error\"}">>,
            {error, Resp}
    end.
