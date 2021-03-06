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
-include("delay.hrl").

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
            case m:find_all_acceptors() of
                {ok, []} ->
                    ok;
                {ok, As = [HA|_]} ->
                    Now = erlang:system_time(seconds),
                    DelKeys = [A#acceptor.pid || A <- As, Now - A#acceptor.created_at > ?MAX_POLLING_TIMEOUT ],
                    m:multi_delete(acceptor, DelKeys),
                    rpc:async_call(HA#acceptor.node, 'delay', send_acceptor, [HA#acceptor.pid]);
                {aborted, Err} ->
                    error_logger:error_report('find_all_acceptors',[{error, Err}])
            end,
            %% Undefined webhook jobs would be executed by dequeue POST api
            {next_state, ready, State};
        Hook ->
            case m:delete(job, Uid) of
                ok  ->
                    post_webhook(Hook, Data),
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

-spec post_webhook(Url :: binary(), Data :: #{}) -> {ok, integer()} | {error, term()}.
post_webhook(Url, Data) ->
    Payload = encode_to_binary(Data),
    case hackney:request(post, Url, [], Payload, [{pool, webhook}]) of
        {ok, Status, _, ClientRef} when Status >= 400-> % error
            io:format("status:~p", [Status]),
            Body = parse_body(ClientRef),
            io:format("body:~p", [Body]),
            error_logger:error_report("error post request to webhook",[{code, Status}, {body, Body}]),
            {error, Status};
        {ok, Status, _, _} ->
            {ok, Status};
        {error, Reason} ->
            error_logger:error_report("failed to connect webhook endpoint",[{webhook, Url}]),
            {error, Reason}
    end.

-spec parse_body(ClientRef :: hackney:client_ref()) -> binary() | atom() | {closed, binary()}.
parse_body(ClientRef) ->
    case hackney:body(ClientRef) of
        {ok, Body} ->
            Body;
        {error, Reason} ->
            error_logger:error_report("error parse webhook response body",[{error, Reason}]),
            Reason;
        Reason ->
            error_logger:error_report("error parse webhook response body",[{error, Reason}]),
            Reason
    end.

-spec encode_to_binary(Data :: #{}) -> binary().
encode_to_binary(Data) ->
    try jiffy:encode(Data) of
        Bin -> Bin
    catch
        _TypeError:_Err ->
            error_logger:error_report("failed to encode post data",[{data, Data}]),
            ?FAILED_ENCODED_BINARY
    end.
