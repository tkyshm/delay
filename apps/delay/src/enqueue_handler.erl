-module(enqueue_handler).

-export([init/2]).
-export([
         content_types_accepted/2,
         allowed_methods/2
        ]).
-export([enqueue/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, enqueue}
     ], Req, State}.

enqueue(Req, State) ->
    case cowboy_req:has_body(Req) of
        true  ->
            %% enqueue
            Body = parse_body(Req, <<>>),
            Data = jiffy:decode(Body, [return_maps]),
            ExecTime = validate_exec_time(Data), %% optional
            Hook = validate_webhook(Data), %% optional
            JobData = decode_job_data(Data), %% decode data
            if
                is_integer(ExecTime) ->
                    Uid = job_sup:spawn_child(JobData, Hook, ExecTime),
                    Resp = <<"{\"job_uid\":\"", Uid/binary, "\"}">>,
                    {stop, cowboy_req:reply(200, #{}, Resp, Req), State};
                true ->
                    Resp = <<"{\"error\":\"Bad request\"}">>,
                    {stop, cowboy_req:reply(400,#{}, Resp, Req), State}
            end;
        false ->
            {stop, cowboy_req:reply(400, Req), State}
    end.

parse_body(Req, Init) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, _} -> <<Init/binary, Data/binary>>;
        {more, Data, _} -> parse_body(Req, <<Init/binary, Data/binary>>)
    end.

validate_exec_time(Data) ->
    case maps:find(<<"exec_time">>, Data) of
        {ok, ExecTime} when is_integer(ExecTime) ->
            ExecTime;
        _ ->
            0
    end.

validate_webhook(Data) ->
    case maps:find(<<"webhook">>, Data) of
        {ok, Hook} when is_binary(Hook) ->
            Hook;
        _ ->
           undefined
    end.

decode_job_data(Data) ->
    case maps:find(<<"data">>, Data) of
        {ok, Hook} ->
            Hook;
        _ ->
           undefined
    end.
