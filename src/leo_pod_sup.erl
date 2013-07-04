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
-module(leo_pod_sup).

-author('Yosuke Hara').

-behaviour(supervisor).

-include("leo_pod.hrl").

%% API
-export([start_link/0, start_link/5, stop/1]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link(?MODULE, []).

start_link(Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs) ->
    supervisor:start_link({local, Id}, ?MODULE,
                          [Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs]).

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
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} };

init([Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs]) ->
    ChildId = ?gen_manager_id(Id),
    %% list_to_atom(lists:append([atom_to_list(Id), "_manager"])),
    ChildSpec = {ChildId,
                 {leo_pod_manager, start_link,
                  [ChildId, PodSize, MaxOverflow, WorkerMod, WorkerArgs]},
                 permanent,
                 2000,
                 worker,
                 [leo_pod_manager]},
    {ok, { {one_for_one, 5, 10}, [ChildSpec]} }.

