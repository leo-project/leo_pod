%%======================================================================
%%
%% Leo-POD
%%
%% Copyright (c) 2014-2015 Rakuten, Inc.
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
-module(basho_bench_driver_leo_pod).

-export([new/1,
         run/4]).

-include_lib("eunit/include/eunit.hrl").

-define(POD_NAME, 'test_worker_pod').
-define(MOD_NAME, 'leo_pod_mod').
-define(FUN_NAME, 'echo_1').

new(1) ->
    PodName = ?POD_NAME,
    PodSize     = 8,
    MaxOverflow = 16,
    ModName     = ?MOD_NAME,
    WorkerArgs  = [{protocol, tcp},
                   {host, "127.0.0.1"},
                   {port, 8080}],
    Ret = leo_pod:start_link(PodName, PodSize, MaxOverflow, ModName, WorkerArgs),
    ?debugVal(Ret),
    {ok, null};
new(_) ->
    {ok, null}.


run(exec,_KeyGen,_ValueGen, State) ->
    {ok, Worker} = leo_pod:checkout(?POD_NAME),
    {ok, _Msg2} = gen_server:call(Worker, {?FUN_NAME, <<"Hello Hal,">>}),

    ok = leo_pod:checkin(?POD_NAME, Worker),
    {ok, State}.

