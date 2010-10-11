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
#ifndef __HAMMY_DB__
#define __HAMMY_DB__

#include "ham/hamsterdb.h"
#include "erl_nif_compat.h"

#define HAMMY_TRUE 1
#define HAMMY_FALSE 0
#define HAMMY_MAX_DB 33

typedef struct _hammy_db_t {
    ham_env_t *env;
    ham_db_t *databases[HAMMY_MAX_DB];
    int db_count;
    char *filename;
    int closed;
} hammy_db;

int hammy_open(ErlNifEnv *env, char *filename, hammy_db *db, int create);
int hammy_close(hammy_db *db);

int hammy_put(ErlNifEnv *env, hammy_db *db, unsigned char *key, int key_size, unsigned char *value, int value_size);
int hammy_get(ErlNifEnv *env, hammy_db *db, unsigned char *key, int key_size, ErlNifBinary *value);
int hammy_del(ErlNifEnv *env, hammy_db *db, unsigned char *key, int key_size);

#endif
