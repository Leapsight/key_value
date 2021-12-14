%% =============================================================================
%%  key_value.erl -
%%
%%  Copyright (c) 2016-2020 Leapsight Holdings Limited. All rights reserved.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc A Key Value coding interface for property lists and maps.
%% @end
%% -----------------------------------------------------------------------------
-module(key_value).

-define(BADKEY, '$error_badkey').

-type t()           ::  map() | [proplists:property()].
-type key()         ::  term() | [term()].

-export_type([t/0]).
-export_type([key/0]).

-export([get/2]).
-export([get/3]).
-export([set/3]).
-export([put/3]).
-export([remove/2]).
-export([take/2]).

-compile({no_auto_import, [get/1]}).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc Returns value `Value' associated with `Key' if `KVTerm' contains `Key'.
%% `Key' can be a term or a path represented as a list of terms.
%%
%% The call fails with a {badarg, `KVTerm'} exception if `KVTerm' is not a
%% property list or map. It also fails with a {badkey, `Key'} exception if no
%% value is associated with `Key'.
%% @end
%% -----------------------------------------------------------------------------
-spec get(Key :: key(), KVTerm :: t()) -> Value :: term().

get(Key, KVTerm) ->
    get(Key, KVTerm, ?BADKEY).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get(Key :: key(), KVTerm :: t(), Default :: term()) -> term().

get([], _, _) ->
    error(badkey);

get(_, [], Default) ->
    maybe_badkey(Default);

get(_, KVTerm, Default) when is_map(KVTerm) andalso map_size(KVTerm) == 0 ->
    maybe_badkey(Default);

get([H|[]], KVTerm, Default) ->
    get(H, KVTerm, Default);

get([H|T], KVTerm, Default) when is_list(KVTerm) ->
    case lists:keyfind(H, 1, KVTerm) of
        {H, Child} ->
            get(T, Child, Default);
        false ->
            maybe_badkey(Default)
    end;

get(Key, KVTerm, Default) when is_list(KVTerm) ->
    case lists:keyfind(Key, 1, KVTerm) of
        {Key, Value} ->
            Value;
        false ->
            maybe_badkey(Default)
    end;

get([H|T], KVTerm, Default) when is_map(KVTerm) ->
    case maps:find(H, KVTerm) of
        {ok, Child} ->
            get(T, Child, Default);
        error ->
            maybe_badkey(Default)
    end;

get(Key, KVTerm, Default) when is_map(KVTerm) ->
    maybe_badkey(maps:get(Key, KVTerm, Default));

get(_, _, _) ->
    error(badarg).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec set(Key :: key(), Value :: any(), KVTerm :: t()) -> NewKVTerm :: t().

set(Key, Value, KVTerm) ->
    put(Key, Value, KVTerm).


%% -----------------------------------------------------------------------------
%% @doc
%% If Key is a tuple it will be treated as a list
%% @end
%% -----------------------------------------------------------------------------
-spec put(Key :: key(), Value :: any(), KVTerm :: t()) -> NewKVTerm :: t().

put([H|[]], Value, KVTerm) ->
    put(H, Value, KVTerm);

put([H|T], Value, KVTerm) when is_list(KVTerm) ->
    InnerTerm = put(T, Value, get(H, KVTerm, [])),
    lists:keystore(H, 1, KVTerm, {H, InnerTerm});

put([H|T], Value, KVTerm) when is_map(KVTerm) ->
    InnerTerm = put(T, Value, get(H, KVTerm, [])),
    maps:put(H, InnerTerm, KVTerm);

put([], _, _)  ->
    error(badkey);

put(Key, Value, KVTerm) when is_list(KVTerm) ->
    lists:keystore(Key, 1, KVTerm, {Key, Value});

put(Key, Value, KVTerm) when is_map(KVTerm) ->
    maps:put(Key, Value, KVTerm);

put(_, _, _) ->
    error(badarg).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec remove(Key :: key(), KVTerm :: t()) -> NewKVTerm :: t().


remove([H|[]], KVTerm) ->
    remove(H, KVTerm);

remove([H|T], KVTerm) when is_list(KVTerm) ->
    InnerTerm = remove(T, get(H, KVTerm, [])),
    lists:keystore(H, 1, KVTerm, {H, InnerTerm});

remove([H|T], KVTerm) when is_map(KVTerm) ->
    InnerTerm = remove(T, get(H, KVTerm, [])),
    maps:put(H, InnerTerm, KVTerm);

remove([], _)  ->
    error(badkey);

remove(Key, KVTerm) when is_list(KVTerm) ->
    lists:keydelete(Key, 1, KVTerm);

remove(Key, KVTerm) when is_map(KVTerm) ->
    maps:remove(Key, KVTerm);

remove(_, _) ->
    error(badarg).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec take(Key :: key(), KVTerm :: t()) ->
    {Value :: term(), NewKVTerm :: t()} | error.


take([H|[]], KVTerm) ->
    take(H, KVTerm);

take([H|T], KVTerm) when is_list(KVTerm) ->
    case take(T, get(H, KVTerm, [])) of
        {Val, InnerTerm} ->
            {Val, lists:keystore(H, 1, KVTerm, {H, InnerTerm})};
        error ->
            error
    end;

take([H|T], KVTerm) when is_map(KVTerm) ->
    case take(T, get(H, KVTerm, [])) of
        {Val, InnerTerm} ->
            {Val, maps:put(H, InnerTerm, KVTerm)};
        error ->
            error
    end;

take([], _)  ->
    error(badkey);

take(Key, KVTerm) when is_list(KVTerm) ->
    case lists:keytake(Key, 1, KVTerm) of
        {value, {Key, Value}, NewKVTerm} ->
            {Value, NewKVTerm};
        false ->
            error
    end;

take(Key, KVTerm) when is_map(KVTerm) ->
    maps:take(Key, KVTerm);

take(_, _) ->
    error(badarg).



%% =============================================================================
%% PRIVATE
%% =============================================================================



%% @private
maybe_badkey(?BADKEY) ->
    error(badkey);

maybe_badkey(Term) ->
    Term.



