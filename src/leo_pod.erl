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
-spec start_link(atom(),non_neg_integer(),non_neg_integer(),module(),[any()],fun((any()) -> any())) -> {'ok',pid()}.
start_link(PodName, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
    leo_pod_sup:start_link(PodName, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun).


%% @doc Stop the worker pool.
%%
-spec stop(atom()) -> 'true' | 'not_started'.
stop(PodName) ->
    leo_pod_sup:stop(PodName).


%% @doc Checkout a worker from the worker pool.
%%
-spec checkout(atom()) -> {ok, pid()}.
checkout(PodName) ->
    leo_pod_manager:checkout(PodName).


%% @doc Checkin a worker into the worker pool.
%%
-spec checkin(atom(), pid()) -> ok.
checkin(PodName, Worker) ->
    leo_pod_manager:checkin(PodName, Worker).

%% @doc Checkin a worker into the worker pool assynchronously.
%%
-spec checkin_async(atom(), pid()) -> ok.
checkin_async(PodName, Worker) ->
    leo_pod_manager:checkin_async(PodName, Worker).

%% @doc Get the status of the worker pool.
%% It returns the tuple of the numbers of working_processes, waiting processes, and room of overflow.
-spec status(atom()) -> {ok, {non_neg_integer(),
                              non_neg_integer(),
                              non_neg_integer()}}.
status(PodName) ->
    leo_pod_manager:status(PodName).

%% ===================================================================
%% Internal functions
%% ===================================================================

