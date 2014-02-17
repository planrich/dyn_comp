#ifndef _SYMT_H_
#define _SYMT_H_

#include "khash.h"
#include "types.h"
#include "error.h"
#include <inttypes.h>

struct __expr_t;
struct __func_t;

enum __sym_entry_type_t {
    SYM_FUNC = 1,
    SYM_ANON_FUNC = 2,
    SYM_VAR = 4,
    SYM_ARG = 8,
};
typedef enum __sym_entry_type_t sym_entry_type_t;

struct __sym_entry_t {
    type_t type;
    sym_entry_type_t entry_type;
    union {
        struct __expr_t * var;
        struct __func_t * func;
    };
    uint8_t argument_index;
    char * param;
};
typedef struct __sym_entry_t sym_entry_t;

KHASH_MAP_INIT_STR(str_sym_entry_t, sym_entry_t);

struct __sym_table_t {
    khash_t(str_sym_entry_t) * symbols;
    struct __sym_table_t * parent;
    struct __sym_table_t * next;
};
typedef struct __sym_table_t sym_table_t;

#define sym_entry_is(entry, symbol) ((entry->entry_type & symbol) != 0) 

sym_table_t * neart_sym_table_alloc();

/**
 * Free this symbol table
 */
void neart_sym_table_free(sym_table_t * table);

/**
 * Make a variable visible in the symbol table
 */
semantic_error_t neart_sym_table_insert(sym_table_t * table, const char * name, sym_entry_t entry);

/**
 * Lookup a symbol. This call searches in all parent symbol tables.
 */
sym_entry_t * neart_sym_table_lookup(sym_table_t * sym, const char * name);

/**
 *  +---------+
 *  | symt 1  |
 *  +---------+
 *      |^
 *      ||
 * next || parent
 *      v|
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
 *      |^
 *      ||
 * next || parent
 *      v|
 *  +--------+
 *  | symt 2 |
 *  +------ -+
 *
 *  will return only symt 1. if param 2 is != 0 symt 2 is freed
 */
sym_table_t * neart_sym_table_pop(sym_table_t * sym, int free);

/**
 * returns -1 if the entry is not pointing to a func/anonfunc
 */
int neart_sym_table_func_param_count(sym_entry_t * e);

#endif /* _SYMT_H_ */
