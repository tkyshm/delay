%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2017, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2017-03-05 18:33:48.553013
%%%-------------------------------------------------------------------
-module(job_worker).

-behaviour(gen_fsm).

%% API
-export([start_link/4]).

%% gen_fsm callbacks
-export([init/1,
         waitting/2,
         ready/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).
-define(MAX_JOB_TIMEOUT, 1000*600). %% 10min interval timeout

-include("schema.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Uid, Data, Hook, ExecTime) ->
    gen_fsm:start_link(?MODULE, [Uid, Data, Hook, ExecTime], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Uid, Data, Hook, ExecTime]) ->
    State = #job{uid = Uid, pid = self(), data = Data, webhook = Hook, exec_time = ExecTime},
    m:store(job, State),
    {ok, 'waitting', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
waitting(_Event, State = #job{exec_time = ExecTime}) ->
    case next_timeout(ExecTime) of
        0 ->
            NewState = State#job{status = ready},
            m:store(job, NewState),
            {next_state, ready, NewState, 0};
        Timeout ->
            {next_state, waitting, State, Timeout}
    end.

ready(dequeue, State = #job{uid = Uid}) ->
    io:format("delete job from mnesia: ~p~n", [Uid]),
    case m:delete(job, Uid) of
        ok ->
            {stop, {shutdown, finished_job}, State};
        Err ->
            error_logger:error_report("error delete job record from mnesia", [{error, Err}]),
            {next_state, ready, State}
    end;
ready(_Event, State = #job{uid = Uid, data = Data, webhook = Hook}) ->
    case Hook of
        undefined ->
            case m:find_all_recievers() of
                [] ->
                    ok;
                [R|_] ->
                    rpc:async_call(R#reciever.node, 'delay', send_reciever, [R#reciever.pid])
            end,
            %% Undefined webhook jobs would be executed by dequeue POST api
            {next_state, ready, State};
        Hook ->
            case m:delete(job, Uid) of
                ok  ->
                    %% TODO: hook response is 200, stop job. other move state ready.
                    EncodedHook = build_webhook(Hook, Data),
                    io:format("hook trigger: ~p~n", [EncodedHook]), % for debug
                    {stop, {shutdown, finished_job}, State};
                Err ->
                    error_logger:error_report("error delete job record from mnesia", [{error, Err}]),
                    {next_state, ready, State}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

% Internal functions
%% Considers a job as being ready if ExecTime is earlier than current unitx time
next_timeout(ExecTime) ->
    UnixTime = erlang:system_time(seconds),
    Timeout = (ExecTime - UnixTime) * 1000, %% converts milliseconds
    if
        Timeout >= ?MAX_JOB_TIMEOUT -> ?MAX_JOB_TIMEOUT;
        Timeout =< 0                -> 0;
        true                        -> Timeout
    end.

build_webhook(Hook, Data) ->
    InitHook = <<Hook/binary, "?">>,
    Url = maps:fold(fun(Key, Val, Acc) ->
        if
            is_integer(Val) ->
                BinVal = integer_to_binary(Val),
                <<Acc/binary, Key/binary, "=", BinVal/binary, "&">>;
            is_binary(Val) ->
                <<Acc/binary, Key/binary, "=", Val/binary, "&">>;
            true ->
                BinVal = jiffy:encode(Val),
                <<Acc/binary, Key/binary, "=", BinVal/binary, "&">>
        end
    end, InitHook, Data),
    erlang:binary_part(Url, {0, byte_size(Url) - 1}).
