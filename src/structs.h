
#ifndef _STRUCTS_H_
#define _STRUCTS_H_

#include "khash.h"
#include "klist.h"

#define FOREACH_EXPR_TYPE(PPF) \
    PPF(ET_UNIT, "unit"), \
    PPF(ET_FUNC, "func"), \
    PPF(ET_PARAMS, "params"), \
    PPF(ET_PARAM, "param"), \
    PPF(ET_PATTERNS, "patterns"), \
    PPF(ET_PATTERN, "pattern"), \
    PPF(ET_VARIABLE, "var"), \
    PPF(ET_BINDINGS, "bindings"), \
    PPF(ET_BINDING, "binding"), \
    PPF(ET_PARENS, "()"), \
    PPF(ET_LIST, "[]"), \
    PPF(ET_CONS, ":"), \
    PPF(ET_MATCH_ANY, "_"), \
    PPF(ET_INTEGER, "int"), \
    PPF(ET_NIL, "nil"), \
    PPF(ET_OP_IADD, "+"), \
    PPF(ET_OP_ISUB, "-"), \
    PPF(ET_OP_IMUL, "*"), \
    PPF(ET_OP_IDIV, "/"), \
    PPF(ET_NEGATIVE, "-"), \

#define ENUM_CNAME(cname, hname) cname
#define ENUM_HNAME(cname, hname) hname

static const char * expr_type_names[] = {

    FOREACH_EXPR_TYPE(ENUM_HNAME)

};

/**
 * => detail
 * -> next
 *
 * + func xyz
 *   +=> params
 *   +-> <func>
 *
 * + params
 *   +=> NULL
 *   +-> param | NULL
 *
 * + param
 *   +=> <expr>
 *   +-> NULL | param
 *
 * + patterns
 *   +=> pattern
 *   +-> NULL
 *
 * + pattern
 *   +=> <expr>
 *   +-> pattern | NULL
 */
typedef enum expr_type_t {
    FOREACH_EXPR_TYPE(ENUM_CNAME)
} expr_type_t;

typedef struct expr_t {
    union {
        struct expr_t * next;
        struct expr_t * right;
    };
    union {
        struct expr_t * detail;
        struct expr_t * left;
    };
    expr_type_t type;
    char * data;
} expr_t;

/**
 * alloc a expression node
 */
expr_t * neart_expr_alloc(expr_type_t type);

/**
 * free the tree recursively
 */
void neart_expr_free_r(expr_t * tree);

/**
 * free one node
 */
void neart_expr_free(expr_t * tree);

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
    const char * filename;
    expr_t * syntax_tree;
} context_t;

context_t * neart_context_alloc(const char * filename);

void neart_context_free(context_t * ctx);

void neart_context_add_function(context_t * ctx, func_t * func);

#endif
