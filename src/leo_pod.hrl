%%======================================================================
%%
%% Leo POD
%%
%% Copyright (c) 2012-2016 Rakuten, Inc.
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

-define(DEF_NUM_OF_CHILDREN, 8).
-type(pod_id() :: atom()).


-define(create_manager_id(_PodId,_ChildId),
        begin
            list_to_atom(
              lists:append([atom_to_list(_PodId), "_", integer_to_list(_ChildId)]))
        end).

-define(get_manager_id(_PodId,_NumOfChildren),
        begin
            {H,S,M} = os:timestamp(),
            Clock = 1000000000000 * H + (S * 1000000 + M),
            _ChildId = (Clock rem _NumOfChildren) + 1,
            ?create_manager_id(_PodId,_ChildId)
        end).
