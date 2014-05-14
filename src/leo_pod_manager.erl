%%======================================================================
%%
%% Leo POD
%%
%% Copyright (c) 2012-2014 Rakuten, Inc.
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
%%======================================================================
-module(leo_pod_manager).

-author('Yosuke Hara').

-behaviour(gen_server).

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

-record(state, {num_of_children = 8 :: pos_integer(),
                max_overflow = 8    :: pos_integer(),
                num_overflow = 8    :: non_neg_integer(),
                worker_mod          :: atom(),
                worker_args = []    :: list(tuple()),
                worker_pids = []    :: list()
               }).


%% ===================================================================
%% API functions
%% ===================================================================
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
-spec(start_link(atom(), pos_integer(), non_neg_integer(),
                 atom(), [any()], function()) ->
             {ok, pid()} | ignore | {error, any()}).
start_link(Id, NumOfChildren, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
    gen_server:start_link({local, Id}, ?MODULE,
                          [NumOfChildren, MaxOverflow, WorkerMod, WorkerArgs, InitFun], []).


-spec(stop(atom()) ->
             ok | {error, any()}).
stop(Id) ->
    gen_server:call(Id, stop, 30000).


%% @doc Check out a worker from a pool
%%
-spec(checkout(atom()) ->
             {ok, pid()} | {error, empty}).
checkout(Id) ->
    gen_server:call(Id, checkout).


%% @doc Check in a worker at a pool
%%
-spec(checkin(atom(), pid()) ->
             ok | {error, any()}).
checkin(Id, WorkerPid) ->
    gen_server:call(Id, {checkin, WorkerPid}).


%% @doc Check in a worker at a pool with asynchronous
%%
-spec(checkin_async(atom(), pid()) -> ok).
checkin_async(Id, WorkerPid) ->
    gen_server:cast(Id, {checkin_async, WorkerPid}).


%% @doc Retrieve the current status in pretty format as follows:
%%      format: { working_process_count,
%%                worker_process_count,
%%                overflow_count }
-spec(status(atom()) ->
             {ok, {non_neg_integer(),
                   non_neg_integer(),
                   non_neg_integer()}}).
status(Id) ->
    gen_server:call(Id, status).


%% @doc Retrieve a raw status of specified Id
%%
-spec(raw_status(atom()) ->
             {ok, [tuple()]} | {error, any()}).
raw_status(Id) ->
    gen_server:call(Id, raw_status).


%% @doc Retrieve pids of specified Id
%%
-spec(pool_pids(atom()) ->
             {ok, [pid()]} | {error, any()}).
pool_pids(Id) ->
    gen_server:call(Id, pool_pids).


%% @doc Retrieve pids of specified Id
%%
-spec(close(atom()) ->
             ok | {error, any()}).
close(Id) ->
    gen_server:call(Id, close).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([NumOfChildren, MaxOverflow, WorkerMod, WorkerArgs, InitFun]) ->
    case InitFun of
        undefined ->
            void;
        _Fun ->
            InitFun(self())
    end,

    case start_child(NumOfChildren, WorkerMod, WorkerArgs, []) of
        {ok, Children} ->
            {ok, #state{num_of_children = NumOfChildren,
                        max_overflow    = MaxOverflow,
                        num_overflow    = MaxOverflow,
                        worker_mod      = WorkerMod,
                        worker_args     = WorkerArgs,
                        worker_pids     = Children}};
        {error, Cause} ->
            {stop, Cause}
    end.

handle_call(stop,_From,State) ->
    {stop, normal, ok, State};

handle_call(checkout, _From, #state{worker_pids  = [],
                                    num_overflow = 0} = State) ->
    {reply, {error, empty}, State};

handle_call(checkout, _From, #state{worker_mod   = WorkerMod,
                                    worker_args  = WorkerArgs,
                                    worker_pids  = [],
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

handle_call(checkout, _From, #state{worker_pids  = Children} = State) ->
    [WorkerPid|NewChildren] = Children,
    {reply, {ok, WorkerPid}, State#state{worker_pids = NewChildren}};

handle_call({checkin, WorkerPid}, _From, #state{num_of_children = NumOfChildren,
                                                num_overflow = NumOverflow,
                                                worker_pids = Children} = State) ->
    case length(Children) >= NumOfChildren of
        true ->
            {reply, ok, State#state{num_overflow = NumOverflow + 1}};
        false ->
            NewChildren = [WorkerPid|Children],
            {reply, ok, State#state{worker_pids = NewChildren}}
    end;

handle_call(status, _From, #state{num_of_children = NumOfChildren,
                                  max_overflow = MaxOverflow,
                                  num_overflow = NumOverflow,
                                  worker_pids = Children} = State) ->
    case length(Children) of
        0 ->
            {reply, {ok, {NumOfChildren + MaxOverflow - NumOverflow,
                          0, NumOverflow}}, State};
        N ->
            {reply, {ok, {NumOfChildren - N, N, MaxOverflow}}, State}
    end;

handle_call(raw_status, _From, State) ->
    {reply, {ok, lists:zip(record_info(fields, state),tl(tuple_to_list(State)))}, State};

handle_call(pool_pids, _From, #state{worker_pids = Children} = State) ->
    {reply, {ok, Children}, State};

handle_call(close, _From, State) ->
    {reply, ok, State#state{worker_pids = []}}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast({checkin_async, WorkerPid}, #state{num_of_children = NumOfChildren,
                                               num_overflow = NumOverflow,
                                               worker_pids = Children} = State) ->
    case length(Children) >= NumOfChildren of
        true ->
            {noreply, State#state{num_overflow = NumOverflow + 1}};
        false ->
            NewChildren = [WorkerPid|Children],
            {noreply, State#state{worker_pids = NewChildren}}
    end;

handle_cast(_, State) ->
    {noreply, State}.


%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
handle_info({'DOWN', MonitorRef, _Type, Pid, _Info}, #state{worker_mod  = WorkerMod,
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


%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.


%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal Functions
%% ===================================================================
%% @doc Start a child-worker
%% @private
-spec(start_child(atom(), [any()]) ->
             {ok, pid()} | {error, any()}).
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
-spec(start_child(non_neg_integer(), atom(), list(any()), [pid()]) ->
             {ok, [pid()]} | {error, any()}).
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

