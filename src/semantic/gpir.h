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
#include "gpir.h"
#include "types.h"
#include <inttypes.h>

//////////////////////////////////////// semantic expr

struct __func_t;

#define FOREACH_SEM_EXPR_CONSTRUCT(P) \
    P(construct_func) \
    P(construct_if) \
    P(construct_then) \
    P(construct_else) \
    P(construct_param) \

#define P1_COLON(C, ...) C,
typedef enum __lang_construct {
    FOREACH_SEM_EXPR_CONSTRUCT(P1_COLON)
} elang_construct_t;

/**
 * +------+     +------+
 * | expr | <-> | expr | <-> ...
 * +------+     +------+
 *    |            |
 *    v            v
 *  NULL        +------+
 *              | expr |
 *              +------+
 *                 |
 *                 v
 *               NULL
 *
 * Then semantic expr (= double acyclic linked list) is a simplified version of the ast tree which is
 * type checked. Each entry can have an optional detail semantic expression which is used for
 * instance for the IF statement.
 */
struct __sem_expr_t {
    struct __sem_expr_t * next;
    struct __sem_expr_t * prev;

    struct __sem_expr_t * detail;

    uint8_t apply; // check if this should reduced in the code generation phase. (func can be arguments)
    type_t type; // the type it has. $ builtin, i interger, g generic
    type_t type_specific;
    elang_construct_t lang_construct; // what construct is it? func|if
    expr_t * expr; // the AST node
    struct __func_t * func; // the function or NULL if it type != type_func, the function otherwise
    int symbol_type; // the type of the entry in the symbol table.
    int argument_index;

    int assigned_register; // this is the register the allocator gave to this expression.
};
typedef struct __sem_expr_t sem_expr_t;

#define __sem_free(x)
KLIST_INIT(sem_expr_t, sem_expr_t, __sem_free)

//////////////////////////////////////// params

typedef int params_t;

typedef short param_offset_t;

typedef char param_t;

struct __pattern_t {
    sem_expr_t * expr;
    sym_table_t * symbols;
    expr_t * bindings;
};

typedef struct __pattern_t pattern_t;

void neart_pattern_free(pattern_t * pattern);

#define __pattern_free(x)
KLIST_INIT(pattern_t, pattern_t*, __pattern_free);

pattern_t * neart_pattern_alloc(sem_expr_t * expr);

//////////////////////////////////////// func

struct __func_t {
    const char * name;
    params_t * params;
    sym_table_t * symbols;

    // can be null if this function just exists for declaration purpose
    klist_t(pattern_t) * patterns;
};
typedef struct __func_t func_t;

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
#define NEART_PARAM_SIZE (2)

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
params_t * neart_params_transform(module_t * module, expr_t * param_expr);

/**
 * (ggii,
 * becomes:
 * 4++++ggii
 *  ||||^^^^
 *  |||+|||+
 *  ||+-||+
 *  |+--|+
 *  +---+
 */
params_t * neart_params_anon_func(param_t * param);

void neart_params_debug_print(params_t * params);

#define neart_params_free(params) free(params);

/**
 * get the paramter at the specific nesting.
 *
 * (a -> [[b]]) -> a -> [b]
 * is: (g0[[g1 g0 g1
 *
 * idx = 0, nesting = 0 -> (a -> [[b]]) == (g0[[g1
 * idx = 0, nesting = 1 -> a == g0
 * idx = 0, nesting = 2 -> [[b]] == [[g1
 * idx = 0, nesting = 3 -> [b] == [g1
 * idx = 0, nesting = 4 -> b == g1
 * idx = 0, nesting = 5 -> NULL
 * idx = 3, nesting = _ -> NULL
 * idx = 2, nesting = 1 -> NULL
 *
 * returns NULL if there is no such parameter.
 */
param_t * neart_param_at(params_t * params, int idx, int nesting);

#define neart_params_count(params) ((*params)-1)
#define neart_params_last(params) (((param_t*)params) + *(((param_offset_t*)(params+1)) + ((*params)-1)))

#define neart_param_type(param) (*param)
#define neart_param_idx(param) ((uint8_t)*(param+1))
#define neart_param_next(param) (param+2)
#define neart_param_end(param) (neart_param_type(param) == ',')

#endif
