%% -------------------------------------------------------------------
%%
%% hammy_nifs: Low-level interface to HamsterDB
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
-module(hammy_nifs).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MISSING_NIF, {error, missing_nif}).
-define(NIF_API_VERSION, 1).

-on_load(init/0).

-export([init/0,
         open/1,
         close/1,
         put/3,
         get/2,
         del/2]).

init() ->
    PrivDir = case code:priv_dir(hammy) of
                  {error, bad_name} ->
                      D = filename:dirname(code:which(?MODULE)),
                      filename:join([D, "..", "priv"]);
                  Dir ->
                      Dir
              end,
    SoName = filename:join([PrivDir, "hammy_nifs"]),
    erlang:load_nif(SoName, ?NIF_API_VERSION).

open(_FilePath) ->
    throw(?MISSING_NIF).

close(_Db) ->
    throw(?MISSING_NIF).

put(_Db, _Key, _Value) ->
    throw(?MISSING_NIF).

get(_Db, _Key) ->
    throw(?MISSING_NIF).

del(_Db, _Key) ->
    throw(?MISSING_NIF).

-ifdef(TEST).
open_test() ->
    DB = unique_name(),
    {ok, R} = hammy_nifs:open(list_to_binary(["/tmp/", DB])),
    ok = hammy_nifs:close(R),
    os:cmd("rm -f /tmp/*" ++ DB ++ "*"),
    ok.

rw_test() ->
    DB = unique_name(),
    {ok, R} = hammy_nifs:open(list_to_binary(["/tmp/", DB])),
    Key = <<"hello">>,
    Value = <<"world">>,
    ok = hammy_nifs:put(R, Key, Value),
    {ok, Value} = hammy_nifs:get(R, Key),
    hammy_nifs:close(R),
    os:cmd("rm -f /tmp/*" ++ DB ++ "*"),
    ok.

rwd_test() ->
    DB = unique_name(),
    {ok, R} = hammy_nifs:open(list_to_binary(["/tmp/", DB])),
    Key = <<"hello">>,
    Value = <<"world">>,
    ok = hammy_nifs:put(R, Key, Value),
    ok = hammy_nifs:del(R, Key),
    {error, notfound} = hammy_nifs:get(R, Key),
    hammy_nifs:close(R),
    os:cmd("rm -f /tmp/*" ++ DB ++ "*"),
    ok.

timing_test() ->
    DB = unique_name(),
    {ok, R} = hammy_nifs:open(list_to_binary(["/tmp/", DB])),
    V = generate_value(1024),
    Start = erlang:now(),
    insert(R, V, 10000),
    End = erlang:now(),
    hammy_nifs:close(R),
    ?debugFmt("10000 inserts took ~p~n", [erlang:round(timer:now_diff(End, Start) / 1000)]),
    os:cmd("rm -f /tmp/*" ++ DB ++ "*"),
    ok.

insert(_R, _V, 0) ->
    ok;
insert(R, V, Count) ->
    Key = unique_val(),
    hammy_nifs:put(R, Key, V),
    insert(R, V, Count - 1).

unique_name() ->
    {_, _, T3} = erlang:now(),
    integer_to_list(T3) ++ ".db".

unique_val() ->
    {_, _, T3} = erlang:now(),
    list_to_binary(integer_to_list(T3)).

generate_value(Size) ->
    generate_value(Size, []).

generate_value(0, Accum) ->
    list_to_binary(Accum);
generate_value(Size, Accum) ->
    C = random:uniform(26) + 95,
    generate_value(Size - 1, [C|Accum]).

-endif.
