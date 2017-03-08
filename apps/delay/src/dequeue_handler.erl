-module(dequeue_handler).

-export([init/2]).
-export([info/3]).
-export([dequeue/0]).
-export([terminate/3]).

-include("schema.hrl").

-define(POLLING_PERIOD, 1000). % 1000 millseconds
-define(DEFAULT_TIMEOUT, 10). % 10 second

init(Req, State) ->
    %%Timeout = cowboy_req:header(<<"x-delay-timeout">>, Req, ?DEFAULT_TIMEOUT) * 1000,
    io:format("test echo"),
    {cowboy_loop, Req, State, hibernate}.

info(timeout, Req, State) ->
    {stop, Req, State};
info(polling, Req, State) ->
    io:format("now: ~p~n",[erlang:system_time(seconds)]),
    case dequeue() of
        {ok, not_found} ->
            {ok, Req, State};
        {ok, Resp} ->
            {ok, cowboy_req:reply(200, #{}, Resp, Req), State};
        {error, ErrResp} ->
            {ok, cowboy_req:reply(500, #{}, ErrResp, Req), State}
    end;
info(Msg, Req, State) ->
    io:format("msg:~p~n", [Msg]),
    {ok, Req, State, hibernate}.

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
