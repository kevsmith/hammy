%% -------------------------------------------------------------------
%%
%% hammy: Erlang key/value store built on top of HamsterDB
%%
%% Copyright (c) 2007-2010 Hypothetical Labs, Inc.  All Rights Reserved.
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
%% -------------------------------------------------------------------
%% @doc This module implements hammy's public API.
%%      Note: Database handles obtained via open/1 and create/1
%%            ARE NOT multi-process safe. This will be fixed in
%%            a later release.
-module(hammy).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([open/1,
         create/1,
         open_or_create/1,
         close/1,
         put/3,
         get/2,
         del/2]).

%% @spec open(list() | binary()) -> {ok, ref()} | {error, open_db}
%% @doc Opens an existing database
open(FileName) when is_list(FileName) ->
    hammy_nifs:open(list_to_binary(FileName));
open(FileName) when is_binary(FileName) ->
    hammy_nifs:open(FileName).

%% @spec create(list() | binary()) -> {ok, ref()} | {error, open_db}
%% @doc Creates a database from scratch. If the database file
%%      already exists this will reset the database resulting in
%%      complete data loss.
create(FileName) when is_list(FileName) ->
    hammy_nifs:create(list_to_binary(FileName));
create(FileName) when is_binary(FileName) ->
    hammy_nifs:create(FileName).

%% @spec open_or_create(list() | binary()) -> {ok, ref()} | {error, open_db}
%% @doc Creates or opens an existing database, as needed. If the filename
%%      exists then it assumed to be a valid database file. If the file
%%      doesn't exist then a new database is created.
open_or_create(FileName) when is_binary(FileName) ->
    open_or_create(binary_to_list(FileName));
open_or_create(FileName) when is_list(FileName) ->
    case filelib:is_dir(FileName) of
        true ->
            erlang:error(badarg);
        false ->
            case filelib:is_file(FileName) of
                true ->
                    open(FileName);
                false ->
                    create(FileName)
            end
    end.

%% @spec close(ref()) -> ok
%% @doc Closes the database. Aborts any pending writes.
close(DB) ->
    hammy_nifs:close(DB).

%% @spec put(ref(), binary(), binary()) -> ok | {error, insert}
%% @doc Inserts a new key/value pair into the database. Will overwrite
%%      existing entries if they exist.
put(DB, Key, Value) when is_binary(Key),
                         is_binary(Value) ->
    hammy_nifs:put(DB, Key, Value).

%% @spec get(ref(), binary()) -> {ok, binary()} | {error, notfound}
%% @doc Retrieves an entry from the database.
get(DB, Key) when is_binary(Key) ->
    hammy_nifs:get(DB, Key).

%% @spec del(ref(), binary()) -> ok
%% @doc Deletes an entry from the database.
del(DB, Key) when is_binary(Key) ->
    hammy_nifs:del(DB, Key).

-ifdef(TEST).
conditional_create_test() ->
    os:cmd("rm -f /tmp/*foo.db*"),
    {ok, R} = hammy:open_or_create("/tmp/foo.db"),
    ok = hammy:put(R, <<"testing">>, <<"123">>),
    ok = hammy:close(R),
    {ok, R1} = open_or_create("/tmp/foo.db"),
    {ok, <<"123">>} = hammy:get(R1, <<"testing">>),
    ok = hammy:close(R1),
    os:cmd("rm -f /tmp/*foo.db*"),
    ok.
-endif.
