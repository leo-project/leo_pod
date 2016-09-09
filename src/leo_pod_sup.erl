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

-behaviour(supervisor).

%% API
-export([start_link/5, start_link/6,
         stop/1]).

-include("leo_pod.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Supervisor callbacks
-export([init/1]).

-record(params, {
          num_of_workers = 1 :: pos_integer(),
          max_overflow = 1 :: pos_integer(),
          worker_mod :: module(),
          worker_args :: [any()],
          init_fun :: function()
         }).


%% ===================================================================
%% API functions
%% ===================================================================
-spec(start_link(Id, NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs) ->
             {ok, WorkerPid::pid()} | ignore | {error, any()} when
      Id::atom(), NumOfWorkers::non_neg_integer(),
      MaxOverflow::non_neg_integer(), WorkerMod::module(), WorkerArgs::[any()]).
start_link(Id, NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs) ->
    Fun = fun(_) -> ok end,
    start_link(Id, NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs, Fun).


-spec(start_link(Id, NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
             {ok, WorkerPid::pid()} | ignore | {error, any()} when
      Id::atom(), NumOfWorkers::non_neg_integer(),
      MaxOverflow::non_neg_integer(), WorkerMod::module(), WorkerArgs::[any()], InitFun::function()).
start_link(Id, NumOfWorkers, MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
    start_link(Id, ?DEF_NUM_OF_CHILDREN, NumOfWorkers,
               MaxOverflow, WorkerMod, WorkerArgs, InitFun).

-spec(start_link(Id, NumOfChildren, NumOfWorkers,
                 MaxOverflow, WorkerMod, WorkerArgs, InitFun) ->
             {ok, WorkerPid::pid()} | ignore | {error, any()} when
      Id::atom(), NumOfWorkers::non_neg_integer(), NumOfChildren::pos_integer(),
      MaxOverflow::non_neg_integer(), WorkerMod::module(), WorkerArgs::[any()], InitFun::function()).
start_link(Id, NumOfChildren, NumOfWorkers, MaxOverflow,
           WorkerMod, WorkerArgs, InitFun) ->
    SupRef = gen_sup_id(Id),
    supervisor:start_link({local, SupRef}, ?MODULE,
                                [Id, NumOfChildren, NumOfWorkers,
                                 MaxOverflow, WorkerMod, WorkerArgs, InitFun]).


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
init([Id, NumOfChildren, NumOfWorkers,
      MaxOverflow, WorkerMod, WorkerArgs, InitFun]) ->
    ok = application:set_env(leo_pod, 'num_of_children', NumOfChildren),

    NumOfWorkers_1 = ?get_num_of_workers_per_child(NumOfWorkers, NumOfChildren),
    Params = #params{num_of_workers = NumOfWorkers_1,
                     max_overflow = MaxOverflow,
                     worker_mod = WorkerMod,
                     worker_args = WorkerArgs,
                     init_fun = InitFun
                    },
    {ok, ChildSpecs} = create_manager_spec(NumOfChildren, Id, Params, []),
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.


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


%% @doc Create leo_pod_manager's spec by pod_id and child_id
-spec(create_manager_spec(ChildId, PodId, Params, Acc) ->
             {ok, Acc} when ChildId::non_neg_integer(),
                            PodId::pod_id(),
                            Params::#params{},
                            Acc::[tuple()]).
create_manager_spec(0,_PodId,_Params, Acc) ->
    {ok, Acc};
create_manager_spec(ChildId, PodId, Params, Acc) ->
    ManagerId =  ?create_manager_id(PodId, ChildId),
    #params{num_of_workers = NumOfWorkers,
            max_overflow = MaxOverflow,
            worker_mod = WorkerMod,
            worker_args = WorkerArgs,
            init_fun = InitFun
           } = Params,
    ChildSpec = {ManagerId,
                 {leo_pod_manager, start_link,
                  [ManagerId, NumOfWorkers, MaxOverflow,
                   WorkerMod, WorkerArgs, InitFun]},
                 permanent,
                 2000,
                 worker,
                 [leo_pod_manager]},
    create_manager_spec(ChildId - 1, PodId, Params, [ChildSpec|Acc]).


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
