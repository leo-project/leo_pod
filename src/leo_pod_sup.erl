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
%%======================================================================
-module(leo_pod_sup).

-author('Yosuke Hara').

-behaviour(supervisor).

%% API
-export([start_link/5, start_link/6,
         stop/1]).

-include_lib("eunit/include/eunit.hrl").

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================
-spec(start_link(atom(), non_neg_integer(),
                 non_neg_integer(), module(), [any()]) ->
             {ok, pid()} | ignore | {error, any()}).
start_link(Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs) ->
    Fun = fun(_) -> ok end,
    start_link(Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs, Fun).

-spec(start_link(atom(), non_neg_integer(),
                 non_neg_integer(), module(), [any()], undefined|function()) ->
             {ok, pid()} | ignore | {error, any()}).
start_link(Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
    SupRef = gen_sup_id(Id),
    supervisor:start_link({local, SupRef}, ?MODULE,
                                [Id, PodSize, MaxOverflow, WorkerMod, WorkerArgs, InitFun]).


-spec(stop(atom()) ->
             ok | not_started).
stop(Id) ->
    SupRef = gen_sup_id(Id),
    case whereis(SupRef) of
        Pid when is_pid(Pid) ->
            List = supervisor:which_children(Pid),
            stop_workers(List);
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


%% ===================================================================
%% Internal Functions
%% ===================================================================
%% @doc Generate supervisor's Id
%% @private
-spec(gen_sup_id(atom() | list()) ->
             atom()).
gen_sup_id(PodId) when is_list(PodId)  ->
    list_to_atom(lists:append([PodId, "_sup"]));
gen_sup_id(PodId) when is_atom(PodId)  ->
    gen_sup_id(atom_to_list(PodId)).


%% @doc Close woker processes
%% @private
-spec(stop_workers([tuple()]) ->
             ok).
stop_workers([]) ->
    ok;
stop_workers([{Id,_Pid,worker,[leo_pod_manager = Mod]}|Rest]) ->
    _ = Mod:close(Id),
    stop_workers(Rest);
stop_workers([_|Rest]) ->
    stop_workers(Rest).
