
#include "symt.h"

#include <errno.h>
#include "error.h"
#include "logging.h"
#include "utils.h"

sym_table_t * neart_sym_table_alloc() {
    ALLOC_STRUCT(sym_table_t, sym);
    sym->symbols = kh_init(str_sym_entry_t);
    sym->parent = sym->next = NULL;
    return sym;
}

void neart_sym_table_free(sym_table_t * table) {

    sym_table_t * t = table;
    sym_table_t * next = table->next;

    while (t != NULL) {
        kh_destroy(str_sym_entry_t, t->symbols); 
        free(t);

        t = next;
        if (t != NULL) {
            next = next->next;
        }
    }
}

void neart_sym_table_insert(sym_table_t * table, const char * name, sym_entry_t entry) {

    int ret;
    khint_t k;

    k = kh_put(str_sym_entry_t, table->symbols, name, &ret);
    if (!ret) {
        errno = ERR_SYM_ALREADY_DEF;
        NEART_LOG(LOG_FATAL, "symbol %s is already defined\n", name);
    } else {
        kh_value(table->symbols, k) = entry;
    }
}

sym_entry_t * neart_sym_table_lookup(sym_table_t * sym, const char * name) {

    sym_table_t * t = sym;
    sym_table_t * next = t->parent;
    khint_t k;

    while (t != NULL) {

        k = kh_get(str_sym_entry_t, t->symbols, name);
        if (k != kh_end(t->symbols)) {
            return & kh_value(t->symbols, k);
        }

        t = next;
        if (t != NULL) {
            t = t->parent;
        }
    }


    return NULL;
}

sym_table_t * neart_sym_table_push(sym_table_t * sym) {
    sym_table_t * new = neart_sym_table_alloc();
    new->parent = sym;
    sym->next = new;
    return new;
}

sym_table_t * neart_sym_table_pop(sym_table_t * sym) {
    sym_table_t * table = sym->parent;
    neart_sym_table_free(table);
    return table;
}
