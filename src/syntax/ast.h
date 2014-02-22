
#ifndef _AST_H_
#define _AST_H_

#include <inttypes.h>
#include "khash.h"
#include "klist.h"

const char * expr_type_names[24];
#define FOREACH_EXPR_TYPE(PPF) \
    PPF(ET_ROOT, "module"), \
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


enum __expr_type_t {
    FOREACH_EXPR_TYPE(ENUM_CNAME)
}; 
typedef enum __expr_type_t expr_type_t;

struct __expr_t {
    union {
        struct __expr_t * next;
        struct __expr_t * right;
    };
    union {
        struct __expr_t * detail;
        struct __expr_t * left;
    };
    expr_type_t type;
    char * data;

    // location data
    int first_line;
    int last_line;
    int first_column;
    int last_column;
};
typedef struct __expr_t expr_t;

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

/**
 * convert the data of that expression to an 32 bit integer
 */
int32_t neart_expr_to_int32(expr_t * expr);

#endif
