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
            DelayTime = validate_delay_time(Data),
            if
                is_binary(Event) andalso is_integer(DelayTime) ->
                    Uid = job_sup:spawn_child(Event, DelayTime),
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

validate_delay_time(Data) ->
    case maps:find(<<"delay_time">>, Data) of
        {ok, DelayTime} when is_integer(DelayTime) ->
            DelayTime;
        _ ->
            invalid_delay_time
    end.



