%% -------------------------------------------------------------------
%%
%% hammy_bench: Simple benchmarking tests for hammy
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
-module(hammy_bench).

-define(DB, <<"/tmp/benchmark.db">>).

-export([start/0,
         write/0,
         read/1]).

start() ->
    Keys = write(),
    read(Keys),
    os:cmd("rm -f /tmp/*benchmark.db*"),
    ok.

write() ->
    {ok, R} = hammy_nifs:create(?DB),
    V = generate_value(1024),
    Start = erlang:now(),
    {ok, Keys} = insert(R, V, 10000),
    hammy_nifs:close(R),
    End = erlang:now(),
    io:format("Wrote 10000 1K objects in ~pms~n", [erlang:round(timer:now_diff(End, Start) / 1000)]),
    Keys.

read(Keys) ->
    {ok, R1} = hammy_nifs:open(?DB),
    Start1 = erlang:now(),
    read(R1, Keys),
    hammy_nifs:close(R1),
    End1 = erlang:now(),
    io:format("Read 10000 1K objects in ~pms~n", [erlang:round(timer:now_diff(End1, Start1) / 1000)]).

generate_value(Size) ->
    generate_value(Size, []).

generate_value(0, Accum) ->
    list_to_binary(Accum);
generate_value(Size, Accum) ->
    C = random:uniform(26) + 95,
    generate_value(Size - 1, [C|Accum]).

read(_R, []) ->
    ok;
read(R, [Key|T]) ->
    {ok, _} = hammy_nifs:get(R, Key),
    read(R, T).

insert(R, V, Count) ->
    insert(R, V, Count, []).

insert(_R, _V, 0, Accum) ->
    {ok, Accum};
insert(R, V, Count, Accum) ->
    Key = unique_val(),
    hammy_nifs:put(R, Key, V),
    insert(R, V, Count - 1, [Key|Accum]).

unique_val() ->
    {_, _, T3} = erlang:now(),
    list_to_binary(integer_to_list(T3)).
