#ifndef _BINDING_H_
#define _BINDING_H_

#include "ast.h"
#include "gpir.h"
#include "error.h"

/**
 * bindings can are nested/wrapped by [] or (_:_)
 */
semantic_error_t neart_declare_all_bindings(compile_context_t * cc, params_t * params, 
        expr_t * binding, int * binding_count);
    

#endif

