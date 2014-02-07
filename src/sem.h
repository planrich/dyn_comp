#ifndef _SEM_H_
#define _SEM_H_

#include "ast.h"
#include "gpir.h"

typedef enum __semantic_error_t {
    ERR_FUNC_ALREADY_DEF,
} semantic_error_t;

module_t * neart_check_semantics(compile_context_t * cc, expr_t * root);

#endif /* _SEM_H */
