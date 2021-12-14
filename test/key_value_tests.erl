%% =============================================================================
%% key_value_tests - test suite.
%%
%% Copyright (c) 2016-1027 Leapsight. All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% =============================================================================
-module(key_value_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).


get_empty_key_test() ->
    KVC = [{a, 1}],
    ?assertError(badkey, key_value:get([], KVC)).

set_empty_key_test() ->
    KVC = [{a, 1}],
    ?assertError(badkey, key_value:set([], 1, KVC)).

get_empty_test() ->
    ?assertError(badkey, key_value:get([], [])),
    ?assertError(badkey, key_value:get([], #{})).

set_empty_test() ->
    ?assertError(badkey, key_value:set([], 1, [])),
    ?assertError(badkey, key_value:set([], 1, #{})).

get_empty_default_test() ->
    ?assertEqual(1, key_value:get(a, [], 1)),
    ?assertEqual(1, key_value:get([a], [], 1)),
    ?assertEqual(1, key_value:get(a, #{}, 1)),
    ?assertEqual(1, key_value:get([a], #{}, 1)).

set_empty_default_test() ->
    ?assertEqual([{a, 1}], key_value:set(a, 1, [])),
    ?assertEqual([{a, 1}], key_value:set([a], 1, [])),
    ?assertEqual(#{a => 1}, key_value:set(a, 1, #{})),
    ?assertEqual(#{a => 1}, key_value:set([a], 1, #{})),
    ?assertError(badkey, key_value:set([], 1, [])),
    ?assertError(badkey, key_value:set([], 1, #{})).

badarg_get_test() ->
    ?assertError(badkey, key_value:get([], 1)),
    ?assertError(badkey, key_value:get([], 1, 2)),
    ?assertError(badarg, key_value:get(a, 1)),
    ?assertError(badarg, key_value:get(a, 1, 2)),
    ?assertError(badkey, key_value:get([b], [])),
    ?assertError(badkey, key_value:get([b], #{})),
    ?assertError(badkey, key_value:get([b], [{a, 1}])).


badarg_set_test() ->
    ?assertError(badkey, key_value:set([], 1, #{a => 1})),
    ?assertError(badarg, key_value:set(a, 1, true)),
    ?assertError(badarg, key_value:set([a, b], 1, [{a , 1}])),
    ?assertError(badarg, key_value:set([a, b], 1, #{a => 1})).


get_1_test() ->
    D = #{e => 1},
    C = [{d, D}],
    B = #{c => C},
    A = [{b, B}],
    KVC = [{a, A}],

    ?assertEqual(A, key_value:get(a, KVC)),
    ?assertEqual(A, key_value:get([a], KVC)),
    ?assertError(badkey, key_value:get({a}, KVC)),
    ?assertEqual(B, key_value:get(b, A)),
    ?assertEqual(B, key_value:get([b], A)),
    ?assertError(badkey, key_value:get({b}, KVC)),
    ?assertEqual(B, key_value:get([a, b], KVC)),
    ?assertError(badkey, key_value:get({a, b}, KVC)),
    ?assertEqual(C, key_value:get([a, b, c], KVC)),
    ?assertEqual(D, key_value:get([a, b, c, d], KVC)),
    ?assertEqual(1, key_value:get([a, b, c, d, e], KVC)).

set_1_test() ->
    D = #{e => 1},
    C = [{d, D}],
    B = #{c => C},
    A = [{b, B}],
    KVC = [{a, A}],

    ?assertEqual(D, key_value:set(e, 1, #{})),
    ?assertEqual(C, key_value:set(d, D, [])),
    ?assertEqual(B, key_value:set(c, C, #{})),
    ?assertEqual(A, key_value:set(b, B, [])),
    ?assertEqual(KVC, key_value:set(a, A, [])).



    %% ?assertEqual(B, key_value:set(b, A)),
    %% ?assertEqual(B, key_value:set([b], A)),
    %% ?assertEqual(B, key_value:set({b}, A)),
    %% ?assertEqual(B, key_value:set([a, b], KVC)),
    %% ?assertEqual(B, key_value:set({a, b}, KVC)),
    %% ?assertEqual(C, key_value:set([a, b, c], KVC)),
    %% ?assertEqual(D, key_value:set([a, b, c, d], KVC)),
    %% ?assertEqual(1, key_value:set([a, b, c, d, e], KVC)).