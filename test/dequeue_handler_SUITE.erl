%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2017, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2017-03-08 00:17:18.472833
%%%-------------------------------------------------------------------
-module(dequeue_handler_SUITE).


%% API
-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         test_dequeue/1,
         test_dequeue_timeout/1
        ]).

-import(test, [stop_delay/0, post_enqueue/1, wait_polling/1]).

-include_lib("common_test/include/ct.hrl").
-include("test.hrl").

all() ->
    [
     test_dequeue,
     test_dequeue_timeout
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 60}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(delay),
    {ok, _} = application:ensure_all_started(gun),
    Config.

end_per_suite(_Config) ->
    stop_delay(),
    application:stop(gun).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

test_dequeue(_Config) ->
    wait_polling(self()),

    Data = #{<<"name">> => <<"test">>, <<"id">> => 1},
    Payload = #{
        <<"data">> => Data,
        <<"event">> => <<"test_event">>,
        <<"exec_time">> => 1489320320
    },
    post_enqueue(Payload),

    receive
        {ok, Resp} ->
            [M] = jiffy:decode(Resp,[return_maps]),
            true = maps:is_key(<<"uid">>, M),
            Data = maps:get(<<"data">>, M)
    after
        5000 ->
            ct:abort_current_testcase(could_not_dequeue)
    end,

    ok.

test_dequeue_timeout(_Config) ->
    {ok, Pid} = gun:open("localhost", ?TEST_DELAY_PORT, #{protocols => [http]}),
    Start = erlang:system_time(seconds),
    Ref = gun:get(Pid, "/api/dequeue", [{<<"x-delay-timeout">>, <<"2">>}]),
    {response, fin, 408, _} = gun:await(Pid, Ref, 60000),
    End = erlang:system_time(seconds),
    2 = End - Start,
    ok.
