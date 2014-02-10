#ifndef _SYMT_H_
#define _SYMT_H_

#include "khash.h"
#include "types.h"

struct expr_t;
struct func_t;

typedef enum __sym_entry_type_t {
    SYM_FUNC,
    SYM_VAR,

} sym_entry_type_t;

typedef struct __sym_entry_t {
    type_t type;
    sym_entry_type_t entry_type;
    union {
        struct expr_t * var;
        struct func_t * func;
    };
} sym_entry_t;
KHASH_MAP_INIT_STR(str_sym_entry_t, sym_entry_t);

typedef struct __sym_table_t {
    khash_t(str_sym_entry_t) * symbols;
    struct __sym_table_t * parent;
    struct __sym_table_t * next;
} sym_table_t;

sym_table_t * neart_sym_table_alloc();

/**
 * Frees all symbol tables and child symbol tables
 */
void neart_sym_table_free(sym_table_t * table);

/**
 * Make a variable visible in the symbol table
 */
void neart_sym_table_insert(sym_table_t * table, const char * name, sym_entry_t entry);

/**
 * Lookup a symbol. This call searches in all parent symbol tables.
 */
sym_entry_t * neart_sym_table_lookup(sym_table_t * sym, const char * name);

/**
 *  +---------+
 *  | symt 1  |
 *  +---------+
 *       ^
 *       |
 *       | parent
 *       |
 *  +--------+
 *  | symt 2 |
 *  +------ -+
 *
 *  symt 2 is created and returned
 */
sym_table_t * neart_sym_table_push(sym_table_t * sym);

/**
 *  +---------+
 *  | symt 1  |
 *  +---------+
 *       ^
 *       |
 *       | parent
 *       |
 *  +--------+
 *  | symt 2 |
 *  +------ -+
 *
 *  will return only symt 1. if param 2 is != 0 symt 2 is freed
 */
sym_table_t * neart_sym_table_pop(sym_table_t * sym, int free);

#endif /* _SYMT_H_ */
