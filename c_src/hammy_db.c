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
#include <stdio.h>
#include <string.h>
#include "hammy_db.h"

#define HAMMY_ENV_FLAGS HAM_ENABLE_TRANSACTIONS

static ham_parameter_t HAMMY_ENV_OPTS[] = {{HAM_PARAM_MAX_ENV_DATABASES, HAMMY_MAX_DB}, {0, NULL}};
static ham_parameter_t HAMMY_DB_OPTS[] = {{HAM_PARAM_KEYSIZE, 255}, {0, NULL}};
static ham_parameter_t EMPTY_OPTS[] = {{0, NULL}};

/* Database & environment functions */
int hammy_setup_env(ErlNifEnv *env, hammy_db *db);
void setup_key(ham_key_t *k, unsigned char *key, int key_size);
void setup_record(ham_record_t *k, unsigned char *value, int value_size);

int hammy_open(ErlNifEnv *env, char *filename, hammy_db *db, int create) {
    int retval = HAMMY_FALSE;
    if (ham_env_new(&(db->env)) == HAM_SUCCESS) {
        if (create) {
            if (ham_env_create_ex(db->env,
                                  filename, HAMMY_ENV_FLAGS, 0600, HAMMY_ENV_OPTS) == HAM_SUCCESS) {
                db->db_count = 0;
                retval = hammy_setup_env(env, db);
                db->filename = filename;
                db->closed = HAMMY_FALSE;
            }
            else {
                ham_env_close(db->env, HAM_AUTO_CLEANUP | HAM_TXN_AUTO_ABORT);
                db->closed = HAMMY_TRUE;
            }
        }
        else {
            if (ham_env_open_ex(db->env, filename, HAMMY_ENV_FLAGS, 0) == HAM_SUCCESS) {
                db->db_count = 0;
                retval = hammy_setup_env(env, db);
                db->filename = filename;
                db->closed = HAMMY_FALSE;
            }
            else {
                ham_env_close(db->env, HAM_AUTO_CLEANUP | HAM_TXN_AUTO_ABORT);
                db->closed = HAMMY_TRUE;
            }
        }
    }
    return retval;
}

int hammy_close(hammy_db *db) {
    if (!db->closed) {
        ham_env_close(db->env, HAM_TXN_AUTO_ABORT | HAM_AUTO_CLEANUP);
        db->closed = HAMMY_TRUE;
    }
    return HAMMY_TRUE;
}

int hammy_put(ErlNifEnv *env, hammy_db *db, unsigned char *key, int key_size, unsigned char *value, int value_size) {
    ham_key_t k;
    ham_record_t rec;
    ham_txn_t *txn;
    int rc;

    setup_key(&k, key, key_size);
    setup_record(&rec, value, value_size);

    ham_txn_begin(&txn, db->databases[0], 0);
    rc = ham_insert(db->databases[0], txn, &k, &rec, HAM_OVERWRITE);
    if (rc == HAM_SUCCESS) {
        ham_txn_commit(txn, 0);
        return HAMMY_TRUE;
    }
    else {
        ham_txn_abort(txn, 0);
        return HAMMY_FALSE;
    }
}

int hammy_get(ErlNifEnv *env, hammy_db *db, unsigned char *key, int key_size, ErlNifBinary *value) {
    ham_key_t k;
    ham_record_t rec;
    ham_txn_t *txn;
    int rc;
    int retval = HAMMY_FALSE;

    setup_key(&k, key, key_size);
    memset(&rec, 0, sizeof(ham_record_t));

    ham_txn_begin(&txn, db->databases[0], HAM_TXN_READ_ONLY);
    rc = ham_find(db->databases[0], txn, &k, &rec, 0);
    if (rc == HAM_SUCCESS) {
        if (enif_alloc_binary_compat(env, rec.size, value)) {
            memcpy(value->data, rec.data, rec.size);
            retval = HAMMY_TRUE;
        }
    }
    ham_txn_commit(txn, 0);
    return retval;
}

int hammy_del(ErlNifEnv *env, hammy_db *db, unsigned char *key, int key_size) {
    ham_key_t k;
    ham_txn_t *txn;
    int retval = HAMMY_FALSE;

    setup_key(&k, key, key_size);

    ham_txn_begin(&txn, db->databases[0], HAM_TXN_READ_ONLY);
    if (ham_erase(db->databases[0], txn, &k, 0) == HAM_SUCCESS) {
        ham_txn_commit(txn, 0);
        retval = HAMMY_TRUE;
    }
    else {
        ham_txn_abort(txn, 0);
    }
    return retval;
}

int hammy_setup_env(ErlNifEnv *env, hammy_db *db) {
    int retval = HAMMY_TRUE;
    ham_size_t count = HAMMY_MAX_DB;
    ham_u16_t *names = (ham_u16_t *) enif_alloc_compat(env, sizeof(ham_u16_t) * HAMMY_MAX_DB);
    ham_new(&(db->databases[0]));
    ham_new(&(db->databases[1]));
    if ( ham_env_get_database_names(db->env, names, &count) == HAM_SUCCESS) {
        /* New env, so let's setup data database and meta database */
        if (count == 0) {
            /* Database 1 is data */
            ham_env_create_db(db->env, db->databases[0], 1, HAM_USE_BTREE, HAMMY_DB_OPTS);
            /* Database 2 is metadata */
            ham_env_create_db(db->env, db->databases[1], 2, HAM_USE_BTREE, HAMMY_DB_OPTS);
            db->db_count = 2;
        }
        else {
            ham_env_open_db(db->env, db->databases[0], 1, 0, EMPTY_OPTS);
            ham_env_open_db(db->env, db->databases[1], 2, 0, EMPTY_OPTS);
            /* TODO: Load existing user-defined indices */
            db->db_count = 2;
        }
    }
    else {
        retval = HAMMY_FALSE;
    }
    enif_free_compat(env, names);
    return retval;
}

void setup_key(ham_key_t *k, unsigned char *key, int key_size) {
    memset(k, 0, sizeof(ham_key_t));
    k->data = key;
    k->size = key_size;
}

void setup_record(ham_record_t *rec, unsigned char *value, int value_size) {
    memset(rec, 0, sizeof(ham_record_t));
    rec->data = value;
    rec->size = value_size;
}
