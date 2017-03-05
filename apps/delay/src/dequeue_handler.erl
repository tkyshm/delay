-module(dequeue_handler).

-export([init/2]).
-export([info/3]).
-export([dequeue/2]).

-include("schema.hrl").

-define(DEFAULT_COUNT, 50).
-define(SERVER_INTERVAL, 200).

init(Req0, _) ->
    Req1 = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"application/json">>}, Req0),
    erlang:send_after(200, self(), timeout),
    {cowboy_loop, Req1, ?DEFAULT_COUNT}. %% TODO: enable to set count

info(timeout, Req, 0) ->
    {stop, Req, 0};
info(timeout, Req, Count) ->
    erlang:send_after(?SERVER_INTERVAL, self(), timeout),
    dequeue(Req, Count).

dequeue(Req, Count) ->
    case m:find_all_ready_jobs() of
        {ok, Jobs} ->
            lists:foreach(fun(J) ->
                gen_fsm:send_event(J#job.pid, dequeue)
            end, Jobs),
            JobJson = lists:map(fun(J) ->
                #{<<"data">> => J#job.data, <<"uid">> => J#job.uid}
            end, Jobs),
            Resp = jiffy:encode(JobJson),
            cowboy_req:stream_body(Resp, nofin, Req),
            {ok, Req, Count - 1};
        _ ->
            Resp = <<"{\"error\":\"Internal Server Error\"}">>,
            cowboy_req:stream_body(Resp, nofin, Req),
            {ok, cowboy_req:reply(500, #{}, Resp, Req), Count}
    end.
