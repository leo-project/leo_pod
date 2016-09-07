%%======================================================================
%%
%% Leo POD
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc leo_pod_manager can manage worker pools in the pod
%% @reference https://github.com/leo-project/leo_pod/blob/master/src/leo_pod_manager.erl
%% @end
%%======================================================================
-module(leo_pod_manager).

-behaviour(gen_server).

-include("leo_pod.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/6,
         stop/1
        ]).

-export([checkout/1,
         checkin/2,
         checkin_async/2,
         status/1,
         raw_status/1,
         pool_pids/1,
         close/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {num_of_workers = 1 :: pos_integer(),
                max_overflow = 1 :: pos_integer(),
                num_overflow = 0 :: non_neg_integer(),
                worker_mod :: atom(),
                worker_args = [] :: list(tuple()),
                worker_pids = [] :: list()
               }).


%% ===================================================================
%% API functions
%% ===================================================================
%% @doc Initialize a wooker pool
%%
-spec(start_link(PodId, NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
             {ok, pid()} | ignore | {error, any()} when PodId :: pod_id(),
                                                        NumOfWorkers :: pos_integer(),
                                                        MaxOverflow :: non_neg_integer(),
                                                        WorkerMod :: module(),
                                                        WorkerArgs :: [any()],
                                                        InitFun :: function()).
start_link(PodId, NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
    gen_server:start_link({local, PodId}, ?MODULE,
                          [NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs, InitFun], []).


%% @doc Stop the worker pool
%%
-spec(stop(PodId) ->
             ok | {error, any()} when PodId :: pod_id()).
stop(PodId) ->
    gen_server:call(PodId, stop, 30000).


%% @doc Check out a worker from the worker pool
%%
-spec(checkout(PodId) ->
             {ok, pid()} | {error, empty} when PodId :: pod_id()).
checkout(PodId) ->
    gen_server:call(PodId, checkout).


%% @doc Check in a worker to the woker pool
%%
-spec(checkin(PodId, WorkerPid) ->
             ok | {error, any()} when PodId :: pod_id(),
                                      WorkerPid :: pid()).
checkin(PodId, WorkerPid) ->
    gen_server:call(PodId, {checkin, WorkerPid}).


%% @doc Check in a worker to the worker pool with asynchronous
%%
-spec(checkin_async(PodId, WorkerPid) ->
             ok when PodId :: pod_id(),
                     WorkerPid :: pid()).
checkin_async(PodId, WorkerPid) ->
    gen_server:cast(PodId, {checkin_async, WorkerPid}).


%% @doc Retrieve the current status in pretty format as follows:
%%      format: { working_process_count,
%%                worker_process_count,
%%                overflow_count }
%% @end
-spec(status(PodId) ->
             {ok, PodState} when PodId::pod_id(),
                                 PodState::#pod_state{}).
status(PodId) ->
    gen_server:call(PodId, status).


%% @doc Retrieve a raw status of specified PodId
%%
-spec(raw_status(PodId) ->
             {ok, [tuple()]} |
             {error, any()} when PodId :: pod_id()).
raw_status(PodId) ->
    gen_server:call(PodId, raw_status).


%% @doc Retrieve pids of specified PodId
%%
-spec(pool_pids(PodId) ->
             {ok, [pid()]} |
             {error, any()} when PodId :: pod_id()).
pool_pids(PodId) ->
    gen_server:call(PodId, pool_pids).


%% @doc Retrieve pids of specified PodId
%%
-spec(close(PodId) ->
             ok | {error, any()} when PodId :: pod_id()).
close(PodId) ->
    gen_server:call(PodId, close).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================
%% @doc gen_server callback - Module:init(Args) -> Result
%%
init([NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs, InitFun]) ->
    InitFun(self()),

    case start_child(NumOfWorkers, WorkerMod, WorkerArgs, []) of
        {ok, Children} ->
            {ok, #state{num_of_workers = NumOfWorkers,
                        max_overflow = MaxOverflow,
                        num_overflow = MaxOverflow,
                        worker_mod = WorkerMod,
                        worker_args = WorkerArgs,
                        worker_pids = Children}};
        {error, Cause} ->
            {stop, Cause}
    end.

%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
%%
handle_call(stop,_From,State) ->
    {stop, normal, ok, State};

handle_call(checkout, _From, #state{worker_pids = [],
                                    num_overflow = 0} = State) ->
    {reply, {error, empty}, State};

handle_call(checkout, _From, #state{worker_mod = WorkerMod,
                                    worker_args = WorkerArgs,
                                    worker_pids = [],
                                    num_overflow = NumOverflow} = State) ->
    {Res, NewState} =
        case start_child(WorkerMod, WorkerArgs) of
            {ok, ChildPid} ->
                {{ok, ChildPid},
                 State#state{num_overflow = NumOverflow - 1}};
            {error, _Cause} ->
                {{error, empty}, State}
        end,
    {reply, Res, NewState};

handle_call(checkout, _From, #state{worker_pids = Children} = State) ->
    [WorkerPid|NewChildren] = Children,
    {reply, {ok, WorkerPid}, State#state{worker_pids = NewChildren}};

handle_call({checkin, WorkerPid}, _From, #state{num_of_workers = NumOfWorkers,
                                                num_overflow = NumOverflow,
                                                worker_pids = Children} = State) ->
    case length(Children) >= NumOfWorkers of
        true ->
            {reply, ok, State#state{num_overflow = NumOverflow + 1}};
        false ->
            NewChildren = [WorkerPid|Children],
            {reply, ok, State#state{worker_pids = NewChildren}}
    end;

handle_call(status, _From, #state{num_of_workers = NumOfWorkers,
                                  max_overflow = MaxOverflow,
                                  num_overflow = NumOverflow,
                                  worker_pids = Children} = State) ->
    case length(Children) of
        0 ->
            {reply, {ok, #pod_state{num_of_working_refs =
                                        NumOfWorkers + MaxOverflow - NumOverflow,
                                    num_of_waiting_refs = 0,
                                    num_of_buffers = NumOverflow}}, State};
        N ->
            {reply, {ok, #pod_state{num_of_working_refs = NumOfWorkers - N,
                                    num_of_waiting_refs = N,
                                    num_of_buffers = MaxOverflow}}, State}
    end;

handle_call(raw_status, _From, State) ->
    {reply, {ok, lists:zip(record_info(fields, state),tl(tuple_to_list(State)))}, State};

handle_call(pool_pids, _From, #state{worker_pids = Children} = State) ->
    {reply, {ok, Children}, State};

handle_call(close, _From, State) ->
    {reply, ok, State#state{worker_pids = []}}.


%% @doc gen_server callback - Module:handle_cast(Request, State) -> Result
%%
handle_cast({checkin_async, WorkerPid}, #state{num_of_workers = NumOfWorkers,
                                               num_overflow = NumOverflow,
                                               worker_pids = Children} = State) ->
    case length(Children) >= NumOfWorkers of
        true ->
            {noreply, State#state{num_overflow = NumOverflow + 1}};
        false ->
            NewChildren = [WorkerPid|Children],
            {noreply, State#state{worker_pids = NewChildren}}
    end;

handle_cast(_, State) ->
    {noreply, State}.


%% @doc gen_server callback - Module:handle_info(Info, State) -> Result
%%
handle_info({'DOWN', MonitorRef, _Type, Pid, _Info}, #state{worker_mod = WorkerMod,
                                                            worker_args = WorkerArgs,
                                                            worker_pids = ChildPids} = State) ->
    true = erlang:demonitor(MonitorRef),

    ChildPids1 = lists:delete(Pid, ChildPids),
    ChildPids2 = case start_child(WorkerMod, WorkerArgs) of
                     {ok, ChildPid} ->
                         [ChildPid|ChildPids1];
                     _ ->
                         ChildPids1
                 end,
    {noreply, State#state{worker_pids = ChildPids2}};

handle_info(_Info, State) ->
    {noreply, State}.


%% @doc gen_server callback - Module:terminate(Reason, State)
%% <p>
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% </p>
%% @end
terminate(_Reason, _State) ->
    ok.

%% @doc gen_server callback - Module:code_change(OldVsn, State, Extra) -> {ok, NewState} | {error, Reason}
%% <p>
%% Description: Convert process state when code is changed
%% </p>
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal Functions
%% ===================================================================
%% @doc Start a child-worker
%% @private
-spec start_child(WorkerMod, WorkerArgs) -> {ok, ChildId} | {error, any()} when
      WorkerMod :: module(),
      WorkerArgs :: [any()],
      ChildId :: pid().

start_child(WorkerMod, WorkerArgs) ->
    case WorkerMod:start_link(WorkerArgs) of
        {ok, ChildPid} ->
            _MonitorRef = erlang:monitor(process, ChildPid),
            {ok, ChildPid};
        {error, Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING}, {function, "start_child/2"},
                                    {line, ?LINE}, {body, Cause}]),
            {error, Cause}
    end.

%% @doc Start multiple child-workers
%% @private
-spec start_child(Index, WorkerMod, WorkerArgs, Children1) -> {ok, Children2} | {error, any()} when
      Index :: non_neg_integer(),
      WorkerMod :: module(),
      WorkerArgs :: [any()],
      Children1 :: [pid()],
      Children2 :: [pid()].

start_child(0,_,_,Children) ->
    {ok, Children};
start_child(Index, WorkerMod, WorkerArgs, Children) ->
    case start_child(WorkerMod, WorkerArgs) of
        {ok, ChildPid} ->
            Children1 = [ChildPid|Children],
            start_child(Index - 1, WorkerMod, WorkerArgs, Children1);
        {error, Cause} ->
            {error, Cause}
    end.
