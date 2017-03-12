-module(test).

-compile(export_all).
-include("test.hrl").

stop_delay() ->
	cowboy:stop_listener(delay_httpd),
	ok = application:stop(mnesia),
	ok = application:stop(hackney),
	ok = application:stop(delay).

start_echo_handler() ->
    Disp = cowboy_router:compile([{'_', [
        {"/webhook", echo_handler, []}
    ]}]),

    cowboy:start_clear(test_echo_handler, 2, [{port, 9120}], #{env => #{dispatch => Disp}}).

post_enqueue(Payload) ->
    Body = jiffy:encode(Payload),
    Header = [{"content-type", "application/json"}],
    Type = "application/json",
    URL = ?TEST_ENQUEUE_ENDPOINT,
    httpc:request(post, {URL, Header, Type, Body}, [], []).

wait_polling(Pid) ->
    F = fun() ->
            {ok, Conn} = gun:open("localhost", ?TEST_DELAY_PORT, #{protocols => [http]}),
            Ref = gun:get(Conn, "/api/dequeue", [{<<"x-delay-timeout">>, <<"1">>}]),
            Res = gun:await_body(Conn, Ref, 1000),
            Pid ! Res
    end,
    spawn_link(F).
