%%====================================================================
%%
%% Leo Pod
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
%%====================================================================
-module(leo_pod_tests).

-author('yosuke hara').

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

all_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun suite_/1
                          ]]}.

setup() ->
    ok.

teardown(_) ->
    ok.

suite_(_) ->
    %% Prepare-1
    PodId = 'test_worker_pod',
    PodSize     = 8,
    MaxOverflow = 16,
    ModName     = 'leo_pod_mod',
    WorkerArgs  = [{protocol, tcp},
                   {host, "127.0.0.1"},
                   {port, 8080}],
    InitFun = fun(_ManagerRef) ->
            void
    end,
    leo_pod:start_link(PodId, PodSize, MaxOverflow, ModName, WorkerArgs, InitFun),

    %% @TODO
    %% Confirm procs #1
    %% ?assertEqual({ok, {0, PodSize, MaxOverflow}}, leo_pod:status(PodId)),

    %% Execute-1 - [checkout > exec > checkin]
    ok = execute_1(10000, PodId, echo),

    %% @TODO
    %% ?assertEqual({ok, {0, PodSize, MaxOverflow}}, leo_pod:status(PodId)),

    %% @TODO
    %% stop a target child proc
    %% {ok, [Pid1|_]} = leo_pod_manager:pool_pids(PodId),
    %% ok = gen_server:call(Pid1, stop),
    %% ?assertEqual({ok, {0, PodSize, MaxOverflow}}, leo_pod:status(PodId)),

    %% Execute-2 - [checkout > exec > checkin]
    ok = execute_2(PodSize + 2, PodId, slow_echo),

    %% @TODO
    %% timer:sleep(100),
    %% ?assertEqual({ok, {PodSize + 2, 0, MaxOverflow - 2}}, leo_pod:status(PodId)),
    %% timer:sleep(300),
    %% ?assertEqual({ok, {0, PodSize, MaxOverflow}}, leo_pod:status(PodId)),

    %% Prepare-2
    PodId_1 = 'test_worker_pod_1',
    PodSize1     = 4,
    MaxOverflow1 = 8,
    ModName1     = 'leo_pod_mod',
    WorkerArgs1  = [{protocol, tcp},
                    {host, "127.0.0.1"},
                    {port, 8080}],
    leo_pod:start_link(PodId_1, PodSize1, MaxOverflow1, ModName1, WorkerArgs1, InitFun),

    %% @TODO
    %% Confirm procs #2
    %% ?assertEqual({ok, {0, PodSize1, MaxOverflow1}}, leo_pod:status(PodId_1)),

    %% Execute-4 - [checkout > exec > checkin]
    ok = execute_1(16, PodId_1, echo),
    %% @TODO
    %% ?assertEqual({ok, {0, PodSize1, MaxOverflow1}}, leo_pod:status(PodId_1)),

    %% Termination
    leo_pod:stop(PodId),
    leo_pod:stop(PodId_1),
    ok.


%% ===================================================================
%% Internal Functions
%% ===================================================================
execute_1(0,_Name,_Fun) ->
    ok;
execute_1(Index, Name, Fun) ->
    {ok, {PodManagerId, Worker}} = leo_pod:checkout(Name),
    %% ?debugVal({PodManagerId, Worker}),

    Msg1 = lists:append(["index_", integer_to_list(Index)]),
    {ok, Msg2} = gen_server:call(Worker, {Fun, Msg1}),
    ?assertEqual(Msg1, Msg2),

    ok = leo_pod:checkin_async(PodManagerId, Worker),
    execute_1(Index - 1, Name, Fun).

execute_2(0,_Name,_Fun) ->
    ok;
execute_2(Index, Name, Fun) ->
    spawn(fun() ->
                  {ok, {PodManagerId, Worker}} = leo_pod:checkout(Name),

                  Msg1 = lists:append(["index_", integer_to_list(Index)]),
                  {ok, Msg2} = gen_server:call(Worker, {Fun, Msg1}),
                  ?assertEqual(Msg1, Msg2),

                  ok = leo_pod:checkin(PodManagerId, Worker)
          end),
    execute_2(Index - 1, Name, Fun).

-endif.
