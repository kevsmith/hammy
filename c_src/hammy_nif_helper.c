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
#include <string.h>

#include "erl_nif_compat.h"

char *hammy_extract_string(ErlNifEnv *env, const ERL_NIF_TERM term) {
  ErlNifBinary bin;
  if (!enif_inspect_binary(env, term, &bin)) {
    return NULL;
  }
  char *buf = (char *) enif_alloc_compat(env, bin.size + 1);
  if (buf == NULL) {
    return NULL;
  }
  memset(buf, 0, bin.size + 1);
  memcpy(buf, bin.data, bin.size);
  return buf;
}

unsigned char *hammy_extract_raw_string(ErlNifEnv *env, const ERL_NIF_TERM term) {
  ErlNifBinary bin;
  if (!enif_inspect_binary(env, term, &bin)) {
    return NULL;
  }
  unsigned char *buf = (unsigned char *) enif_alloc_compat(env, bin.size + 1);
  if (buf == NULL) {
    return NULL;
  }
  memset(buf, 0, bin.size + 1);
  memcpy(buf, bin.data, bin.size);
  return buf;
}

int hammy_extract_binary(ErlNifEnv *env, const ERL_NIF_TERM term, ErlNifBinary *bin) {
  if (!enif_inspect_binary(env, term, bin)) {
    return 0;
  }
  else {
    return 1;
  }
}

ERL_NIF_TERM hammy_make_erl_string(ErlNifEnv *env, const char *str, ERL_NIF_TERM *target) {
  int size = strlen(str) + 1;
  char *buf = enif_alloc_compat(env, size);
  if (buf == NULL) {
    return 0;
  }
  memset(buf, 0, size);
  memcpy(buf, str, size - 1);
  *target = enif_make_string(env, buf, ERL_NIF_LATIN1);
  return 1;
}

void hammy_maybe_free(ErlNifEnv *env, void *thing) {
    if (thing != NULL) {
        enif_free_compat(env, thing);
    }
    thing = NULL;
}
