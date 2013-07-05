%%======================================================================
%%
%% Leo POD
%%
%% Copyright (c) 2012-2013 Rakuten, Inc.
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
-export([start_link/5,
         stop/1
        ]).

-export([checkout/1,
         checkin/2,
         status/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {num_of_children = 0 :: pos_integer(),
                max_overflow = 0    :: pos_integer(),
                worker_mod          :: atom(),
                worker_args = []    :: list(tuple()),
                worker_pids = []    :: list()
               }).


%% ===================================================================
%% API functions
%% ===================================================================
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Id, NumOfChildren, MaxOverflow, WorkerMod, WorkerArgs) ->
    gen_server:start_link({local, Id}, ?MODULE,
                          [NumOfChildren, MaxOverflow, WorkerMod, WorkerArgs], []).

stop(Id) ->
    gen_server:call(Id, stop, 30000).

checkout(Id) ->
    gen_server:call(Id, checkout).

checkin(Id, WorkerPid) ->
    gen_server:call(Id, {checkin, WorkerPid}).

status(Id) ->
    gen_server:call(Id, status).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([NumOfChildren, MaxOverflow, WorkerMod, WorkerArgs]) ->
    {ok, Children} =
        start_child(NumOfChildren, WorkerMod, WorkerArgs, []),
    {ok, #state{num_of_children = NumOfChildren,
                max_overflow    = MaxOverflow,
                worker_mod      = WorkerMod,
                worker_args     = WorkerArgs,
                worker_pids     = Children}}.

handle_call(stop,_From,State) ->
    {stop, normal, ok, State};

handle_call(checkout, _From, #state{worker_mod   = WorkerMod,
                                    worker_args  = WorkerArgs,
                                    worker_pids  = Children,
                                    max_overflow = MaxOverflow} = State) ->
    {Res, NewState} =
        case Children of
            [] ->
                case MaxOverflow > 0 of
                    true ->
                        case start_child(WorkerMod, WorkerArgs) of
                            {ok, ChildPid} ->
                                {{ok, ChildPid},
                                 State#state{max_overflow = MaxOverflow - 1}};
                            {error, _Cause} ->
                                {{error, empty}, State}
                        end;
                    false ->
                        {{error, empty}, State}
                end;
            _ ->
                [WorkerPid|NewChildren] = Children,
                {{ok, WorkerPid}, State#state{worker_pids = NewChildren}}
        end,
    {reply, Res, NewState};

handle_call({checkin, WorkerPid}, _From, #state{worker_pids = Children} = State) ->
    NewChildren = [WorkerPid|Children],
    {reply, ok, State#state{worker_pids = NewChildren}};

handle_call(status, _From, State) ->
    {reply, {ok, State#state.worker_pids}, State}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
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
-spec(start_child(atom(), list(any())) ->
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

-spec(start_child(integer(), atom(), list(any()), list(pid())) ->
             {ok, pid()} | {error, any()}).
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

