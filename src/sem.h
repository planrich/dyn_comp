#ifndef _SEM_H_
#define _SEM_H_

#include "ast.h"
#include "gpir.h"

module_t * neart_check_semantics(compile_context_t * cc, expr_t * root, int * error);

#endif /* _SEM_H_ */
