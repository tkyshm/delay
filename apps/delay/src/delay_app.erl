%%%-------------------------------------------------------------------
%% @doc delay public API
%% @end
%%%-------------------------------------------------------------------

-module(delay_app).

-behaviour(application).

%% Application callbacks
-export([start/2, profile_output/0, stop/1]).

-define(DEFAULT_PORT, 9811).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_profile(),
    case application:get_env(port) of
        {ok, Port} -> start_api_server(Port);
        _ -> start_api_server(?DEFAULT_PORT)
    end,
    delay_sup:start_link().

profile_output() ->
    eprof:stop_profiling(),
    eprof:log("epms_procs.profile"),
    eprof:analyze(procs),
    eprof:log("epms_total.profile"),
    eprof:analyze(total).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_profile() ->
    case application:get_env(profile) of
        {ok, true} ->
            {ok, _Pid} = eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.

start_api_server(Port) ->
    Dispatch = cowboy_router:compile([
        { '_', [
            { "/api/enqueue", enqueue_handler, [] },
            { "/api/dequeue", dequeue_handler, [] }
        ]}
    ]),
    {ok, _} = cowboy:start_clear(delay_httpd, 100, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }).



