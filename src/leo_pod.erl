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

-author('Yosuke Hara').

%% API
-export([start_link/6,
         stop/1,
         checkout/1,
         checkin/2,
         checkin_async/2,
         status/1
        ]).


%% ===================================================================
%% API functions
%% ===================================================================
%% @doc Initialize a work pool.
%%
-spec(start_link(PodName, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
             {'ok', pid()} when PodName :: atom(),
                                PodSize :: non_neg_integer(),
                                MaxOverflow :: non_neg_integer(),
                                WorkerMod :: module(),
                                WorkerArgs :: [any()],
                                InitFun :: function()).
start_link(PodName, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
    leo_pod_sup:start_link(PodName, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun).


%% @doc Stop the worker pool.
%%
-spec(stop(PodName) ->
             'true' | 'not_started' when PodName :: atom()).
stop(PodName) ->
    leo_pod_sup:stop(PodName).


%% @doc Checkout a worker from the worker pool.
%%
-spec(checkout(PodName) ->
             {ok, pid()} when PodName :: atom()).
checkout(PodName) ->
    leo_pod_manager:checkout(PodName).


%% @doc Checkin the worker into the worker pool.
%%
-spec(checkin(PodName, Worker) ->
             ok when PodName :: atom(),
                     Worker  :: pid()).
checkin(PodName, Worker) ->
    leo_pod_manager:checkin(PodName, Worker).


%% @doc Checkin the worker into the worker pool assynchronously.
%%
-spec(checkin_async(PodName, Worker) ->
             ok when PodName :: atom(),
                     Worker  :: pid()).
checkin_async(PodName, Worker) ->
    leo_pod_manager:checkin_async(PodName, Worker).

%% @doc Get the status of the worker pool.
%%      It returns the tuple of the numbers of working_processes, waiting processes, and room of overflow.
%% @end
-spec(status(PodName) ->
             {ok, {NumOfWorking, NumOfWating,
                   NumOfRoomForOverflow}} when PodName :: atom(),
                                               NumOfWorking :: non_neg_integer(),
                                               NumOfWating  :: non_neg_integer(),
                                               NumOfRoomForOverflow :: non_neg_integer()).
status(PodName) ->
    leo_pod_manager:status(PodName).
