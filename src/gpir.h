#ifndef _GPIR_H_
#define _GPIR_H_

/**
 * General Purpose Internal Representation
 *
 * this ir is not tied to any machine arch. but is mostly intended
 * to validate the parameters, transform the expr into postfix notation
 */

#include "ast.h"
#include "symt.h"
#include "types.h"

typedef struct pattern_t {
    klist_t(expr_t) * expr_postfix;
} pattern_t;

void neart_pattern_free(pattern_t * pattern);

#define __pattern_free(x)
KLIST_INIT(pattern_t, pattern_t*, __pattern_free);

pattern_t * neart_pattern_alloc(klist_t(expr_t) * postfix);

//////////////////////////////////////// func

typedef struct func_t {
    const char * name;
    klist_t(expr_t) * params;

    // can be null if this function just exists for declaration purpose
    klist_t(pattern_t) * patterns;
} func_t;

KHASH_MAP_INIT_STR(str_func_t, func_t*)

#define __func_free(x)
KLIST_INIT(func_t, func_t*, __func_free)


/**
 * create a function with a name
 */
func_t * neart_func_alloc(const char * name);

void neart_func_free(func_t * func);

void neart_func_add_pattern(func_t * func, pattern_t * pattern);

//////////////////////////////////////// module

typedef struct module_t {
    khash_t(str_func_t) * func_table;
    sym_table_t * symbols;
    const char * name;
} module_t;
KHASH_MAP_INIT_STR(str_module_t, module_t*)

module_t * neart_module_alloc(const char * name);

void neart_module_free(module_t * module);

//////////////////////////////////////// compile context

typedef struct compile_context_t {
    khash_t(str_func_t) * qualified_func_table;
    khash_t(str_module_t) * qualified_module_table;

    sym_table_t * symbols;
} compile_context_t;

/**
 * add a function to the module and register it in the compile context.
 *
 * check the errno. it is set when the funciton in this module is already defined.
 */
void neart_module_add_function(compile_context_t * cc, module_t * ctx, func_t * func);

//////////////////////////////////////// params

typedef int params_t;

typedef short param_offset_t;

typedef char param_t;

/**
 * Given the following parameters of a function:
 *
 * (a -> int) -> (int -> a) -> [int] -> [int]
 *
 * they are represented the following way:
 * 
 *  +---> offset
 *  |
 * 4++++(gi,(ig,[i,[i
 *  ||||^   ^   ^  ^
 *  |||+|---|---|--+
 *  ||+-|---|---+
 *  |+--|---+
 *  +---+
 *
 * int first_match_second(type * t2, type * t2) {
 *     if (*t1 == G) return 1;
 *     if (*t1 == P && *t2 == P) return first_match_second(t1+1,t2+1);
 *     if (*t1 == L && *t2 == L) return first_match_second(t1+1,t2+1);
 *     if (*t1 == *t2) return 1;
 *     return 0;
 * }
 */
params_t * neart_params_transform(module_t * module, expr_t * param_expr, int * param_count);

#define neart_params_free(params) free(params);

#endif
