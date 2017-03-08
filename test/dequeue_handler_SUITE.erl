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
         test_dequeue/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     %% TODO: Group names here e.g. {group, crud}
     test_dequeue
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(delay),
    {ok, _} = application:ensure_all_started(gun),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(delay).

init_per_testcase(test_dequeue, Config) ->
    {ok, Pid} = gun:open("localhost", 9020, #{protocols => [http]}),
    Body = <<"{\"event\":\"hoge\",\"exec_time\":1488897936, \"data\":{\"name\":\"aiueo\", \"id\": \"12304\"}}">>,
    {ok, Ref} = gun:post(Pid, "/api/enqueue", #{<<"content-type">> => <<"application/json">>}, Body),

    %{response, fin, 200, _} = gun:await(Pid, Ref),
    Config.

end_per_testcase(test_dequeue, _Config) ->
    ok.

test_dequeue(_Config) ->
    {ok, Pid} = gun:open("localhost", 9020, #{protocols => [http]}),
    Ref = gun:get(Pid, "/api/dequeue", [<<"accept-encoding">>, <<"gzip">>]),
    {response, fin, 200, _} = gun:await(Pid, Ref),
    ok.
