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
%% @doc API of leo_pod
%% @reference https://github.com/leo-project/leo_pod/blob/master/src/leo_pod.erl
%% @end
%%======================================================================
-module(leo_pod).

%% API
-export([start_link/6,
         stop/1,
         checkout/1,
         checkin/2,
         checkin_async/2,
         status/1
        ]).

-include("leo_pod.hrl").
-include_lib("eunit/include/eunit.hrl").


%% ===================================================================
%% API functions
%% ===================================================================
%% @doc Initialize a work pool.
%%
-spec(start_link(PodId, NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
             {'ok', pid()} when PodId :: pod_id(),
                                NumOfWorkers :: non_neg_integer(),
                                MaxOverflow :: non_neg_integer(),
                                WorkerMod :: module(),
                                WorkerArgs :: [any()],
                                InitFun :: function()).
start_link(PodId, NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
    leo_pod_sup:start_link(PodId, NumOfWorkers, MaxOverflow,
                           WorkerMod, WorkerArgs, InitFun).


%% @doc Stop the worker pool.
%%
-spec(stop(PodId) ->
             'true' | 'not_started' when PodId :: pod_id()).
stop(PodId) ->
    leo_pod_sup:stop(PodId).


%% @doc Checkout a worker from the worker pool.
%%
-spec(checkout(PodId) ->
             {ok, pid()} when PodId :: pod_id()).
checkout(PodId) ->
    PodManagerId = ?get_manager_id(PodId, ?env_num_of_children()),
    case leo_pod_manager:checkout(PodManagerId) of
        {ok, WorkerPid} ->
            {ok, {PodManagerId, WorkerPid}};
        Other ->
            Other
    end.


%% @doc Checkin the worker into the worker pool.
%%
-spec(checkin(PodManagerId, Worker) ->
             ok when PodManagerId :: pod_id(),
                     Worker  :: pid()).
checkin(PodManagerId, Worker) ->
    leo_pod_manager:checkin(PodManagerId, Worker).


%% @doc Checkin the worker into the worker pool assynchronously.
%%
-spec(checkin_async(PodManagerId, Worker) ->
             ok when PodManagerId :: pod_id(),
                     Worker  :: pid()).
checkin_async(PodManagerId, Worker) ->
    leo_pod_manager:checkin_async(PodManagerId, Worker).

%% @doc Get the status of the worker pool.
%%      It returns the tuple of the numbers of working_processes, waiting processes, and room of overflow.
-spec(status(PodId) ->
             {ok, [PodState]} when PodId::pod_id(),
                                   PodState::tuple()).
status(PodId) ->
    NumOfChildren = ?env_num_of_children(),
    status_1(NumOfChildren, PodId, []).

%% @private
status_1(0,_, Acc) ->
    {ok, Acc};
status_1(ChildId, PodId, Acc) ->
    ManagerPodId = ?create_manager_id(PodId, ChildId),
    case catch leo_pod_manager:status(ManagerPodId) of
        {ok, PodStatus} ->
            PodStatus_1 =
                lists:zip(record_info(fields, pod_state),tl(tuple_to_list(PodStatus))),
            status_1(ChildId - 1, PodId, [{ManagerPodId, PodStatus_1}|Acc]);
        {_,_Cause} ->
            %% @TODO - output a error message(warn)
            status_1(ChildId - 1, PodId, Acc)
    end.
