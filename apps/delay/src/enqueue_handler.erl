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
            Event = validate_event(Data),
            ExecTime = validate_exec_time(Data),
            Hook = validate_webhook(Data),
            if
                is_binary(Event) andalso is_integer(ExecTime) ->
                    Uid = job_sup:spawn_child(Event, Hook, ExecTime),
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

validate_event(Data) ->
    case maps:find(<<"event">>, Data) of
        {ok, Event} when is_binary(Event) ->
            Event;
        _ ->
            invalid_event
    end.

validate_exec_time(Data) ->
    case maps:find(<<"exec_time">>, Data) of
        {ok, ExecTime} when is_integer(ExecTime) ->
            ExecTime;
        _ ->
            invalid_exec_time
    end.

validate_webhook(Data) ->
    case maps:find(<<"webhook">>, Data) of
        {ok, Hook} when is_binary(Hook) ->
            Hook;
        _ ->
           undefined
    end.
