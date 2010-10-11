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
#include <stdio.h>

#include "erl_nif_compat.h"
#include "hammy_nif_helper.h"
#include "hammy_db.h"

static ErlNifResourceType *hammy_db_RESOURCE;


ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_NOTFOUND;
ERL_NIF_TERM ATOM_DELETE;
ERL_NIF_TERM ATOM_OPEN_DB;
ERL_NIF_TERM ATOM_INSERT;
ERL_NIF_TERM ATOM_OOM;

ERL_NIF_TERM OOM_ERROR;

static int hammy_on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);

// NIF functions
ERL_NIF_TERM hammy_nifs_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hammy_nifs_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hammy_nifs_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hammy_nifs_put(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hammy_nifs_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hammy_nifs_del(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc hammy_nif_funcs[] =
{
    {"open", 1, hammy_nifs_open},
    {"create", 1, hammy_nifs_create},
    {"close", 1, hammy_nifs_close},
    {"put", 3, hammy_nifs_put},
    {"get", 2, hammy_nifs_get},
    {"del", 2, hammy_nifs_del}
};

ERL_NIF_INIT(hammy_nifs, hammy_nif_funcs, &hammy_on_load, NULL, NULL, NULL);

ERL_NIF_TERM hammy_nifs_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    char *filename;
    hammy_db *db;
    ERL_NIF_TERM retval;
    if (argc != 1 || (filename = hammy_extract_string(env, argv[0])) == NULL) {
        return enif_make_badarg(env);
    }

    db = enif_alloc_resource_compat(env, hammy_db_RESOURCE,
                                    sizeof(hammy_db));

    /* Whoops, couldn't allocate resource handle */
    if (db == NULL) {
        enif_release_resource_compat(env, db);
        enif_free_compat(env, filename);
        retval = OOM_ERROR;
    }
    else {
        if (hammy_open(env, filename, db, HAMMY_FALSE)) {
            ERL_NIF_TERM result = enif_make_resource(env, db);
            enif_release_resource_compat(env, db);
            retval = enif_make_tuple2(env, ATOM_OK, result);
        }
        else {
            enif_release_resource_compat(env, db);
            enif_free_compat(env, filename);
            retval = enif_make_tuple2(env, ATOM_ERROR, ATOM_OPEN_DB);
        }
    }
    return retval;
}

ERL_NIF_TERM hammy_nifs_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    char *filename;
    hammy_db *db;
    ERL_NIF_TERM retval;
    if (argc != 1 || (filename = hammy_extract_string(env, argv[0])) == NULL) {
        return enif_make_badarg(env);
    }

    db = enif_alloc_resource_compat(env, hammy_db_RESOURCE,
                                    sizeof(hammy_db));

    /* Whoops, couldn't allocate resource handle */
    if (db == NULL) {
        enif_release_resource_compat(env, db);
        enif_free_compat(env, filename);
        retval = OOM_ERROR;
    }
    else {
        if (hammy_open(env, filename, db, HAMMY_TRUE)) {
            ERL_NIF_TERM result = enif_make_resource(env, db);
            enif_release_resource_compat(env, db);
            retval = enif_make_tuple2(env, ATOM_OK, result);
        }
        else {
            enif_release_resource_compat(env, db);
            enif_free_compat(env, filename);
            retval = enif_make_tuple2(env, ATOM_ERROR, ATOM_OPEN_DB);
        }
    }
    return retval;
}

ERL_NIF_TERM hammy_nifs_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    hammy_db *db;
    if (argc != 1 || !enif_get_resource(env, argv[0], hammy_db_RESOURCE, (void **) &db) ||
        db->closed) {
        return enif_make_badarg(env);
    }
    hammy_close(db);
    return ATOM_OK;
}

ERL_NIF_TERM hammy_nifs_put(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    hammy_db *db;
    ErlNifBinary key, value;
    if (argc != 3 || !enif_get_resource(env, argv[0], hammy_db_RESOURCE, (void **) &db) ||
        db->closed) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_binary(env, argv[1], &key) || !enif_inspect_binary(env, argv[2], &value)) {
        return enif_make_badarg(env);
    }
    if (hammy_put(env, db, key.data, key.size, value.data, value.size)) {
        return ATOM_OK;
    }
    else {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_INSERT);
    }
}

ERL_NIF_TERM hammy_nifs_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    hammy_db *db;
    ErlNifBinary key, value;
    if (argc != 2 || !enif_get_resource(env, argv[0], hammy_db_RESOURCE, (void **) &db) ||
        db->closed) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_binary(env, argv[1], &key)) {
        return enif_make_badarg(env);
    }
    if (hammy_get(env, db, key.data, key.size, &value)) {
        return enif_make_tuple2(env, ATOM_OK, enif_make_binary(env, &value));
    }
    else {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOTFOUND);
    }
}

ERL_NIF_TERM hammy_nifs_del(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    hammy_db *db;
    ErlNifBinary key;
    if (argc != 2 || !enif_get_resource(env, argv[0], hammy_db_RESOURCE, (void **) &db) ||
        db->closed) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_binary(env, argv[1], &key)) {
        return enif_make_badarg(env);
    }
    else {
        if (hammy_del(env, db, key.data, key.size)) {
            return ATOM_OK;
        }
        else {
            return enif_make_tuple2(env, ATOM_ERROR, ATOM_DELETE);
        }
    }
}

void hammy_nifs_db_resource_cleanup(ErlNifEnv *env, void *arg) {
    hammy_db *db = (hammy_db *) arg;
    hammy_close(db);
    hammy_maybe_free(env, db->filename);
}

static int hammy_on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    hammy_db_RESOURCE = enif_open_resource_type_compat(env, "hammy_db_resource",
                                                       &hammy_nifs_db_resource_cleanup,
                                                       ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, 0);
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_NOTFOUND = enif_make_atom(env, "notfound");
    ATOM_DELETE = enif_make_atom(env, "delete");
    ATOM_OPEN_DB = enif_make_atom(env, "open_db");
    ATOM_INSERT = enif_make_atom(env, "insert");
    ATOM_OOM = enif_make_atom(env, "out_of_memory");

    /* Pre-alloate OOM error in case we run out of memory later */
    OOM_ERROR = enif_make_tuple2(env, ATOM_ERROR, ATOM_OOM);
    return 0;
}
