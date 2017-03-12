%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2017, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2017-03-12 21:00:28.592882
%%%-------------------------------------------------------------------
-module(enqueue_handler_SUITE).


%% API
-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

%% test cases
-export([
         %% TODO: test case names go here
         test_enqueue/1,
         test_enqueue_with_webhook/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include("test.hrl").

-import(test, [stop_delay/0, start_echo_handler/0, post_enqueue/1]).

-define(TEST_HANDLER_URL, "http://localhost:9021/webhook").

all() ->
    [
     %% TODO: Group names here e.g. {group, crud}
     test_enqueue,
     test_enqueue_with_webhook
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(delay),
    {ok, _} = start_echo_handler(),
    Config.

end_per_suite(_Config) ->
    cowboy:stop_listener(test_echo_handler),
    stop_delay().

test_enqueue(_Config) ->
    Data = #{<<"name">> => <<"test">>, <<"id">> => 1},
    Payload = #{
        <<"data">> => Data,
        <<"event">> => <<"test_event">>,
        <<"exec_time">> => 1489320320
    },

    {ok, {{_, 200, _}, _, Resp}} = post_enqueue(Payload),
    D = jiffy:decode(Resp,[return_maps]),
    true = maps:is_key(<<"job_uid">>, D),

    ok.

test_enqueue_with_webhook(_Config) ->
    Hook = <<"http://localhost:9120/webhook">>,
    Data = #{<<"name">> => <<"test_enqueue_with_webhook">>,
             <<"id">> => 1},
    Payload = #{
        <<"data">> => Data,
        <<"event">> => <<"test_event">>,
        <<"exec_time">> => 1489320320,
        <<"webhook">> => Hook
    },

    register(test_enqueue_with_webhook, self()),

    {ok, {{_, 200, _}, _, Resp}} = post_enqueue(Payload),
    D = jiffy:decode(Resp,[return_maps]),
    true = maps:is_key(<<"job_uid">>, D),

    %% checks to delivery 'Data' to webhook
    receive
        {webhook_ok, Data} ->
            ok
    after
        5000 ->
          ct:abort_current_testcase(could_not_reached_webhook)
    end,

    ok.
