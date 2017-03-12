-module(echo_handler).

-export([init/2]).
-export([maybe_echo/3]).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	Req2 = maybe_echo(Method, HasBody, Req),
    {ok, Req2, Opts}.

maybe_echo(<<"POST">>, true, Req) ->
	{ok, Body, Req2} = cowboy_req:read_body(Req),
    Data = jiffy:decode(Body, [return_maps]),
    Name = maps:get(<<"name">>, Data),
    PidName = erlang:binary_to_existing_atom(Name, utf8),
    PidName ! {webhook_ok, Data},
	cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req2).
