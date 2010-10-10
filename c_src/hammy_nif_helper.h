// -------------------------------------------------------------------
//
// hammy: An Erlang binding for hamsterdb
//
// Copyright (c) 2010 Hypothetical Labs, Inc. All Rights Reserved.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------
#ifndef __HAMMY_NIF_HELPER__
#define __HAMMY_NIF_HELPER__

#include "erl_nif_compat.h"

char *hammy_extract_string(ErlNifEnv *env, const ERL_NIF_TERM term);

unsigned char *hammy_extract_raw_string(ErlNifEnv *env, const ERL_NIF_TERM term);

int hammy_extract_binary(ErlNifEnv *env, const ERL_NIF_TERM term, ErlNifBinary *bin);

ERL_NIF_TERM hammy_make_erl_string(ErlNifEnv *env, const char *str, ERL_NIF_TERM *target);

void hammy_maybe_free(ErlNifEnv *env, void *thing);

#endif
