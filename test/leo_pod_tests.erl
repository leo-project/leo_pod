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
    PodName = 'test_worker_pod',
    PodSize = 8,
    MaxOverflow = 16,
    ModName = 'leo_pod_mod',
    %% WorkerArgs = [{protocol, tcp},
    %%                {host, "127.0.0.1"},
    %%                {port, 8080}],
    WorkerArgs  = [],
    InitFun = fun(_ManagerRef) ->
                      void
              end,
    leo_pod:start_link(PodName, PodSize, MaxOverflow, ModName, WorkerArgs, InitFun),

    %% Confirm procs #1
    ?assertEqual({ok, {0, PodSize, MaxOverflow}}, leo_pod:status(PodName)),

    %% Execute-1 - [checkout > exec > checkin]
    ok = execute_1(10000, PodName, echo),

    ?assertEqual({ok, {0, PodSize, MaxOverflow}}, leo_pod:status(PodName)),

    %% stop a target child proc
    {ok, [Pid1|_]} = leo_pod_manager:pool_pids(PodName),
    ok = gen_server:call(Pid1, stop),
    ?assertEqual({ok, {0, PodSize, MaxOverflow}}, leo_pod:status(PodName)),

    %% Execute-2 - [checkout > exec > checkin]
    ok = execute_2(PodSize + 2, PodName, slow_echo),
    timer:sleep(100),
    ?assertEqual({ok, {PodSize + 2, 0, MaxOverflow - 2}}, leo_pod:status(PodName)),
    timer:sleep(300),
    ?assertEqual({ok, {0, PodSize, MaxOverflow}}, leo_pod:status(PodName)),

    %% Prepare-2
    PodName_1 = 'test_worker_pod_1',
    PodSize_1 = 2,
    MaxOverflow_1 = 2,
    ModName_1 = 'leo_pod_mod',
    WorkerArgs_1 = [],
    leo_pod:start_link(PodName_1, PodSize_1, MaxOverflow_1, ModName_1, WorkerArgs_1, InitFun),

    %% Confirm procs #2
    ?assertEqual({ok, {0, PodSize_1, MaxOverflow_1}}, leo_pod:status(PodName_1)),

    %% Execute-4 - [checkout > exec > checkin]
    ok = execute_1(16, PodName_1, echo),
    ?assertEqual({ok, {0, PodSize_1, MaxOverflow_1}}, leo_pod:status(PodName_1)),

    %% Execute-2 - [checkout > exec > checkin]
    ok = execute_2(PodSize + 2, PodName_1, slow_echo),
    timer:sleep(timer:seconds(1)),
    ?debugVal(leo_pod:status(PodName_1)),

    %% Termination
    leo_pod:stop(PodName),
    leo_pod:stop(PodName_1),
    ok.


%% ===================================================================
%% Internal Functions
%% ===================================================================
%% @private
execute_1(0,_Name,_Fun) ->
    ok;
execute_1(Index, Name, Fun) ->
    {ok, Worker} = leo_pod:checkout(Name),

    Msg1 = lists:append(["index_", integer_to_list(Index)]),
    {ok, Msg2} = gen_server:call(Worker, {Fun, Msg1}),
    ?assertEqual(Msg1, Msg2),

    ok = leo_pod:checkin_async(Name, Worker),
    execute_1(Index - 1, Name, Fun).

%% @private
execute_2(0,_Name,_Fun) ->
    ok;
execute_2(Index, Name, Fun) ->
    spawn(fun() ->
                  checkout(Index, Name, Fun, 0)
          end),
    execute_2(Index - 1, Name, Fun).

%% @private
checkout(Index, PodName, Fun, Times) ->
    case leo_pod:checkout(PodName) of
        {ok, Worker} ->
            Msg1 = lists:append(["index_", integer_to_list(Index)]),
            {ok, Msg2} = gen_server:call(Worker, {Fun, Msg1}),
            case Times > 0 of
                true ->
                    ?debugVal({Index, {Worker, Msg2}, Times});
                false ->
                    void
            end,
            ?assertEqual(Msg1, Msg2),

            ok = leo_pod:checkin(PodName, Worker);
        {error,_Cause} ->
            ?debugVal({Index, _Cause, Times}),
            timer:sleep(100),
            checkout(Index, PodName, Fun, Times + 1)
    end.

-endif.
