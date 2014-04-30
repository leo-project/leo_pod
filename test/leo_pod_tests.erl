%%====================================================================
%%
%% Leo Pod
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
    PodName = 'test_worker_pod',
    PodSize     = 8,
    MaxOverflow = 16,
    ModName     = 'leo_pod_mod',
    WorkerArgs  = [{protocol, tcp},
                   {host, "127.0.0.1"},
                   {port, 8080}],
    InitFun = fun(_ManagerRef) ->
            void
    end,
    leo_pod:child_spec(PodName, PodSize, MaxOverflow, ModName, WorkerArgs, InitFun),

    %% Confirm procs #1
    {ok, State1} = leo_pod_manager:pool(PodName),
    State1Len = length(State1),
    ?assertEqual(PodSize, State1Len),

    %% Execute-1 - [checkout > exec > checkin]
    ok = execute_1(10000, PodName, echo_1),

    {ok, State2} = leo_pod_manager:pool(PodName),
    State2Len = length(State2),
    ?assertEqual(PodSize, State2Len),

    %% stop a target child proc
    {ok, [Pid1|_]} = leo_pod_manager:pool(PodName),
    ok = gen_server:call(Pid1, stop),

    {ok, State3} = leo_pod_manager:pool(PodName),
    State3Len = length(State3),
    ?assertEqual(PodSize, State3Len),

    %% Execute-2 - [checkout > exec > checkin]
    ok = execute_2(10, PodName, echo_2),
    timer:sleep(100),
    {ok, State4} = leo_pod_manager:status(PodName),
    {max_overflow, MaxOverflow4} = lists:keyfind(max_overflow, 1, State4),
    ?assertEqual(true, MaxOverflow4 < MaxOverflow),
    timer:sleep(300),
    {ok, State5} = leo_pod_manager:pool(PodName),
    State5Len = length(State5),
    ?assertEqual(PodSize, State5Len),

    %% Prepare-2
    PodName1 = 'test_worker_pod_1',
    PodSize1     = 4,
    MaxOverflow1 = 8,
    ModName1     = 'leo_pod_mod',
    WorkerArgs1  = [{protocol, tcp},
                    {host, "127.0.0.1"},
                    {port, 8080}],
    leo_pod:child_spec(PodName1, PodSize1, MaxOverflow1, ModName1, WorkerArgs1, InitFun),

    %% Confirm procs #2
    {ok, State6} = leo_pod_manager:pool(PodName1),
    State6Len = length(State6),
    ?assertEqual(PodSize1, State6Len),

    %% Execute-4 - [checkout > exec > checkin]
    ok = execute_1(16, PodName1, echo_1),

    {ok, State7} = leo_pod_manager:pool(PodName1),
    State7Len = length(State7),
    ?assertEqual(PodSize1, State7Len),
    ok.


%% ===================================================================
%% Internal Functions
%% ===================================================================
execute_1(0,_Name,_Fun) ->
    ok;
execute_1(Index, Name, Fun) ->
    {ok, Worker} = leo_pod:checkout(Name),

    Msg1 = lists:append(["index_", integer_to_list(Index)]),
    {ok, Msg2} = gen_server:call(Worker, {Fun, Msg1}),
    ?assertEqual(Msg1, Msg2),

    ok = leo_pod:checkin_async(Name, Worker),
    execute_1(Index - 1, Name, Fun).

execute_2(0,_Name,_Fun) ->
    ok;
execute_2(Index, Name, Fun) ->
    spawn(fun() ->
                  {ok, Worker} = leo_pod:checkout(Name),

                  Msg1 = lists:append(["index_", integer_to_list(Index)]),
                  {ok, Msg2} = gen_server:call(Worker, {Fun, Msg1}),
                  ?assertEqual(Msg1, Msg2),

                  ok = leo_pod:checkin(Name, Worker)
          end),
    execute_2(Index - 1, Name, Fun).

-endif.
