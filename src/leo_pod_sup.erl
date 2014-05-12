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
-module(leo_pod_sup).

-author('Yosuke Hara').

-behaviour(supervisor).

%% API
-export([start_link/5, start_link/6, stop/1]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================
start_link(Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs) ->
    start_link(Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs, undefined).

start_link(Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
    SupRef = list_to_atom(lists:append([ atom_to_list(Id), "_sup"])),
    supervisor:start_link({local, SupRef}, ?MODULE,
                          [Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun]).

stop(Id) ->
    case whereis(Id) of
        Pid when is_pid(Pid) ->
            exit(Pid, shutdown);
        _ ->
            not_started
    end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun]) ->
    ChildSpec = {Id,
                 {leo_pod_manager, start_link,
                  [Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun]},
                 permanent,
                 2000,
                 worker,
                 [leo_pod_manager]},
    {ok, { {one_for_one, 5, 10}, [ChildSpec]} }.

