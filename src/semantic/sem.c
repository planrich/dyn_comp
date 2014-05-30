
#include "sem.h"

#include <stdio.h>
#include <errno.h>
#include "klist.h"
#include "utils.h"
#include "logging.h"
#include "types.h"
#include "typecheck.h"
#include "binding.h"

#define BAIL_OUT(stmt,label) \
            errno = 0; \
            stmt; \
            if (errno) { goto label; }

// protodefs
static void _check_func_semantics(compile_context_t * cc, module_t * module, func_t * func, expr_t * expr);
static void _check_pattern_semantics(compile_context_t * cc,
        module_t * module,
        func_t * func,
        expr_t * pattern,
        int param_count,
        int pattern_idx);
static void _func_context_add(compile_context_t * cc, module_t * module, klist_t(func_t) * funcs, expr_t * func);

module_t * neart_check_semantics(compile_context_t * cc, expr_t * root) {

    module_t * module = neart_module_alloc(root->data);
    module->symbols = neart_sym_table_push(cc->symbols);

    klist_t(func_t) * funcs = kl_init(func_t);
    kliter_t(func_t) * fit;

    // add the function to the current symbol
    // and transform the parameters to an internal format
    ITER_EXPR_NEXT(root->detail, cur, it)
        if (cur->type == ET_FUNC) {
            BAIL_OUT(_func_context_add(cc, module, funcs, cur), bail_out_sem_check)
        }
    ITER_EXPR_END(it)

    // check the patterns of the function and do transformations
    fit = kl_begin(funcs);
    ITER_EXPR_NEXT(root->detail, cur, it)
        if (cur->type == ET_FUNC) {
            func_t * function = kl_val(fit);

            // some sanity check
            if (strcmp(function->name, cur->data) != 0) {
                errno = ERR_INTERNAL_ENUMERATION_WRONG;
                goto bail_out_sem_check;
            }

            BAIL_OUT(_check_func_semantics(cc, module, function, cur), bail_out_sem_check);
            fit = kl_next(fit);
        }
    ITER_EXPR_END(it)

    kl_destroy(func_t, funcs);
    return module;

bail_out_sem_check:

    NEART_LOG_FATAL("semantic check failed!\n");

    kl_destroy(func_t, funcs);
    return NULL; 
}

/**
 * add the function to the current symbol
 * and transform the parameters to an internal format
 */
static void _func_context_add(compile_context_t * cc,
        module_t * module,
        klist_t(func_t) * funcs,
        expr_t * func) {

    const char * func_name = func->data;
    expr_t * params_expr = func->detail;

    func_t * function = neart_func_alloc(func_name);
    neart_module_add_function(cc, module, function);

    if (params_expr != NULL) {
        function->params = neart_params_transform(module, params_expr->detail);
        //NEART_LOG_DEBUG("func: %s has %d param(s)\n", func_name, neart_params_count(function->params));
    }

    *kl_pushp(func_t, funcs) = function;
}

static void _check_func_semantics(compile_context_t * cc,
        module_t * module,
        func_t * func,
        expr_t * expr) {

    expr_t * patterns = expr->detail->next;

    int param_count = *func->params;
    int pattern_idx = 0;
    ITER_EXPR_NEXT(patterns, cur, it)
        if (cur->type == ET_PATTERN) {
            BAIL_OUT(_check_pattern_semantics(cc, module, func, cur, param_count, pattern_idx++), bail_out_func_sem)
        }
    ITER_EXPR_END(it)
    return;
bail_out_func_sem:
    return;
}

static void _check_pattern_semantics(compile_context_t * cc, 
        module_t * module, 
        func_t * func, 
        expr_t * pattern, 
        int param_count, 
        int pattern_idx) {

    expr_t * bindings = pattern->detail;
    expr_t * expr = bindings->next;
    bindings->next = NULL; // unhinge -> free of syntax tree should not free the expressions

    // override the context within this pattern
    sym_table_t * table = neart_sym_table_push(cc->symbols);
    cc->symbols = table;

    // bindings
    int binding_count = 0;
    if (bindings != NULL) {
        if (neart_declare_all_bindings(cc, func->params, bindings->detail, &binding_count)) {
            goto bail_out_pat;
        }
    }

    // type check the expression
    errno = 0;
    sem_expr_t * spe = neart_type_check(cc, expr, neart_params_last(func->params));
    if (errno) { goto bail_out_pat; }

    sem_expr_t * cur = spe;
    while (cur != NULL && cur->prev != NULL) { cur = cur->prev; }

    pattern_t * pat = neart_pattern_alloc(cur);
    pat->bindings = bindings->detail;
    bindings->detail = NULL; // unhinge to prevent deletion

    neart_func_add_pattern(func,pat);
    pat->symbols = cc->symbols;

    cc->symbols = neart_sym_table_pop(cc->symbols, 0);

    return;
bail_out_pat:
    bindings->next = expr;
}

