-module(dequeue_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([dequeue/2]).

-include("schema.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, dequeue}
     ], Req, State}.

dequeue(Req, State) ->
    case m:find_all_ready_jobs() of
        {ok, Jobs} ->
            lists:foreach(fun(J) ->
                gen_fsm:send_event(J#job.pid, dequeue)
            end, Jobs),
            JobJson = lists:map(fun(J) ->
                #{<<"data">> => J#job.data, <<"uid">> => J#job.uid}
            end, Jobs),
            Resp = jiffy:encode(JobJson),
            {stop, cowboy_req:reply(200, #{}, Resp, Req), State};
        _ ->
            Resp = <<"{\"error\":\"Internal Server Error\"}">>,
            {stop, cowboy_req:reply(500, #{}, Resp, Req), State}
    end.
