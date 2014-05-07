
#include "symt.h"

#include <errno.h>
#include <inttypes.h>
#include "logging.h"
#include "utils.h"
#include "ast.h"
#include "gpir.h"


sym_table_t * neart_sym_table_alloc() {
    ALLOC_STRUCT(sym_table_t, sym);
    sym->symbols = kh_init(str_sym_entry_t);
    sym->parent = sym->next = NULL;
    return sym;
}

void neart_sym_table_free(sym_table_t * table) {
    kh_destroy(str_sym_entry_t, table->symbols); 
    free(table);
}

semantic_error_t neart_sym_table_insert(sym_table_t * table, const char * name, sym_entry_t entry) {

    int ret;
    khint_t k;

    k = kh_put(str_sym_entry_t, table->symbols, name, &ret);
    if (!ret) {
        NEART_LOG_FATAL("symbol %s is already defined\n", name);
        return ERR_SYM_ALREADY_DEF;
    } else {
        kh_value(table->symbols, k) = entry;
    }
    return NO_ERROR;
}

sym_entry_t * neart_sym_table_lookup(sym_table_t * sym, const char * name) {

    khint_t k;

    while (sym != NULL) {
        k = kh_get(str_sym_entry_t, sym->symbols, name);
        if (k != kh_end(sym->symbols)) {
            return & kh_value(sym->symbols, k);
        }

        sym = sym->parent;
    }

    return NULL;
}

sym_table_t * neart_sym_table_push(sym_table_t * sym) {
    sym_table_t * new = neart_sym_table_alloc();
    new->parent = sym;
    sym->next = new;
    return new;
}

sym_table_t * neart_sym_table_pop(sym_table_t * sym, int free) {
    sym_table_t * table = sym->parent;
    if (free) { neart_sym_table_free(sym); }
    return table;
}

int neart_sym_table_func_param_count(sym_entry_t * e) {

    if (sym_entry_is(e, SYM_FUNC)) {
        func_t * func = e->func;
        return neart_params_count(func->params);
    } else if (sym_entry_is(e, SYM_ANON_FUNC)) {
        param_t * param = e->param;
        return neart_param_idx(param) - 1;
    }

    return -1;
}
