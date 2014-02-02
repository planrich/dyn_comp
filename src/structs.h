
#ifndef _STRUCTS_H_
#define _STRUCTS_H_

#include "khash.h"
#include "klist.h"

typedef enum expr_type_t {
    TT_UNIT,
    TT_FUNC,
    TT_PATS,
    TT_LET,
    TT_ADD,
    TT_SUB,
    TT_MUL,
    TT_DIV,
    TT_LEAF
} expr_type_t;

typedef struct expr_t {
    struct expr_t * left;
    struct expr_t * right;
    expr_type_t type;
    char * leaf;
} expr_t;

/**
 * alloc a expression node
 */
expr_t * expr_alloc(expr_type_t type);

/**
 * free the tree recursively
 */
void expr_free_r(expr_t * tree);

/**
 * free one node
 */
void expr_free(expr_t * tree);

#define __expr_free(x)
KLIST_INIT(expr_t, expr_t*, __expr_free);

typedef struct pattern_t {
    klist_t(expr_t) * bindings;
    expr_t * expr;
} pattern_t;

#define __pattern_free(x)
KLIST_INIT(pattern_t, pattern_t*, __pattern_free);

pattern_t * pattern_alloc(void);

void pattern_free(pattern_t * pattern);

void pattern_add_binding(pattern_t * pattern, expr_t * expr);

////////////////////////////////////////

typedef struct func_t {
    const char * name;
    klist_t(expr_t) * params;
    klist_t(pattern_t) * patterns;
} func_t;

KHASH_MAP_INIT_STR(str_func_t, func_t*)

/**
 * create a function with a name
 */
func_t * func_alloc(const char * name);

void func_add_param(func_t * func, expr_t * param);

void func_add_pattern(func_t * func, pattern_t * pattern);

void func_free(func_t * func);

////////////////////////////////////////

typedef struct context_t {
    khash_t(str_func_t) * func_table;

    // the following are needed because a bison rules cannot have a parameter
    // thus they are set 'globally' in the context. One could have solved
    // this by just using the same structure (expr_t) for the syntax tree
    //
    // the last added function
    func_t * last_func;
    // the last created pattern
    pattern_t * last_pattern;

} context_t;

context_t * context_alloc(void);

void context_free(context_t * ctx);

void context_add_function(context_t * ctx, func_t * func);

#endif
