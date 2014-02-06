#ifndef _GPIR_H_
#define _GPIR_H_

/**
 * General Purpose Internal Representation
 *
 * this ir is not tied to any machine arch. but is mostly intended
 * to validate the parameters, transform the expr into postfix notation
 */

#include "ast.h"

#define __expr_free(x) neart_expr_free
KLIST_INIT(expr_t, expr_t*, __expr_free);

typedef struct pattern_t {
    klist_t(expr_t) * expr_postfix;
    expr_t * expr;

} pattern_t;

void neart_pattern_free(pattern_t * pattern);

#define __pattern_free(x) neart_pattern_free
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

/**
 * create a function with a name
 */
func_t * neart_func_alloc(const char * name);

void neart_func_free(func_t * func);

void neart_func_add_pattern(func_t * func, pattern_t * pattern);

//////////////////////////////////////// module

typedef struct module_t {
    khash_t(str_func_t) * func_table;
    const char * name;
} module_t;
KHASH_MAP_INIT_STR(str_module_t, module_t*)

module_t * neart_module_alloc(const char * name);

void neart_module_free(module_t * module);

//////////////////////////////////////// compile context

typedef struct compile_context_t {
    khash_t(str_func_t) * qualified_func_table;
    khash_t(str_module_t) * qualified_module_table;
} compile_context_t;

void neart_module_add_function(compile_context_t * cc, module_t * ctx, func_t * func);

#endif
