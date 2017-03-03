-module(dequeue_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([dequeue/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, dequeue}
     ], Req, State}.

dequeue(Req, State) ->
    Body = <<"{}">>,
    {Body, Req, State}.

