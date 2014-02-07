
#ifndef _AST_H_
#define _AST_H_

#include "khash.h"
#include "klist.h"

const char * expr_type_names[24];
#define FOREACH_EXPR_TYPE(PPF) \
    PPF(ET_ROOT, "root"), \
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
    PPF(ET_STRING, "str"), \
    PPF(ET_NIL, "nil"), \
    PPF(ET_OP_IADD, "+"), \
    PPF(ET_OP_ISUB, "-"), \
    PPF(ET_OP_IMUL, "*"), \
    PPF(ET_OP_IDIV, "/"), \
    PPF(ET_NEGATIVE, "-"), \
    PPF(ET_OP_CONS, ":"), \

#define ENUM_CNAME(cname, hname) cname
#define ENUM_HNAME(cname, hname) hname


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

#define __expr_free(x) 
KLIST_INIT(expr_t, expr_t*, __expr_free);

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



#endif
