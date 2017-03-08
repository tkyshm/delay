-module(dequeue_handler).

-export([init/2]).
-export([info/3]).
-export([dequeue/0]).
-export([terminate/3]).

-include("schema.hrl").

-define(DEFAULT_TIMEOUT, 10). % 10 second

init(Req, State) ->
    Timeout = cowboy_req:header(<<"x-delay-timeout">>, Req, ?DEFAULT_TIMEOUT) * 1000,
    Ua = cowboy_req:header(<<"user-agent">>, Req, <<"">>),
    {Ip, Port} = cowboy_req:peer(Req),
    m:store(reciever, #reciever{pid = self(), node = node(), ip = Ip, port = Port, ua = Ua}),
    erlang:send_after(Timeout, self(), timeout),
    {cowboy_loop, Req, State, hibernate}.

info(timeout, Req, State) ->
    {stop, Req, State};
info(dequeue, Req, State) ->
    case dequeue() of
        {ok, not_found} ->
            {ok, Req, State, hibernate};
        {ok, Resp} ->
            {ok, cowboy_req:reply(200, #{}, Resp, Req), State, hibernate};
        {error, ErrResp} ->
            {ok, cowboy_req:reply(500, #{}, ErrResp, Req), State, hibernate}
    end;
info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
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
