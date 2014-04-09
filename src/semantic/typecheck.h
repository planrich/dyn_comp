
#ifndef _TYPECHECK_H_
#define _TYPECHECK_H_

#include "types.h"
#include "ast.h"
#include "symt.h"
#include "gpir.h"

/**
 * Type check the given expression. Types are matched and the tree is returned
 * as a postfix expr.
 */
sem_expr_t * neart_type_check(compile_context_t * cc, expr_t * expr, param_t * expected_result);

void neart_sem_post_expr_free(sem_expr_t * expr);

#endif
