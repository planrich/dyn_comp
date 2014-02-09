
#include "sem.h"

#include <stdio.h>
#include <errno.h>
#include "klist.h"
#include "utils.h"
#include "logging.h"
#include "types.h"


/**
 * Validate the function semantics.
 * This includes the params, and each binding.
 *
 * errno
 */
static void _check_func_semantics(compile_context_t * cc, module_t * module, func_t * func, expr_t * expr);

/**
 * Validate the pattern semantics of the given item.
 *
 * errno
 */
static void _check_pattern_semantics(compile_context_t * cc,
        module_t * module,
        func_t * func,
        expr_t * pattern,
        int param_count,
        int pattern_idx);
/**
 * Create the function structure on the heap and attach it to the module and compile context
 */
static void _func_context_add(compile_context_t * cc, module_t * module, klist_t(func_t) * funcs, expr_t * func);

/**
 * Transform a expression tree into a postifx list
 */
static void _to_postfix(klist_t(expr_t) * stack, expr_t * expr);

module_t * neart_check_semantics(compile_context_t * cc, expr_t * root) {
    NEART_LOG_TRACE();

    module_t * module = neart_module_alloc(root->data);

    klist_t(func_t) * funcs = kl_init(func_t);
    kliter_t(func_t) * fit;

    ITER_EXPR_NEXT(root->detail, cur, it)
        if (cur->type == ET_FUNC) {
            errno = 0;
            _func_context_add(cc, module, funcs, cur);
            if (errno) { goto bail_out_sem_check; }
        }
    ITER_EXPR_END(it)


    fit = kl_begin(funcs);

    ITER_EXPR_NEXT(root->detail, cur, it)
        if (cur->type == ET_FUNC) {
            func_t * function = kl_val(fit);

            // some sanity check
            if (strcmp(function->name, cur->data) != 0) {
                errno = ERR_INTERNAL_ENUMERATION_WRONG;
                goto bail_out_sem_check;
            }


            errno = 0;
            _check_func_semantics(cc, module, function, cur);
            if (errno) { goto bail_out_sem_check; }
            fit = kl_next(fit);
        }
    ITER_EXPR_END(it)

    kl_destroy(func_t, funcs);
    return module;

bail_out_sem_check:

    kl_destroy(func_t, funcs);
    return NULL; 
}

static void _func_context_add(compile_context_t * cc,
        module_t * module,
        klist_t(func_t) * funcs,
        expr_t * func) {

    const char * func_name = func->data;

    func_t * function = neart_func_alloc(func_name);
    neart_module_add_function(cc, module, function);

    *kl_pushp(func_t, funcs) = function;

}

static void _check_func_semantics(compile_context_t * cc,
        module_t * module,
        func_t * func,
        expr_t * expr) {

    NEART_LOG_TRACE();

    const char * func_name = func->name;
    expr_t * params_expr = expr->detail;
    expr_t * patterns = params_expr->next;

    int param_count = 0;
    if (params_expr != NULL) {
        params_t * params = neart_params_transform(params_expr->detail, &param_count);
        NEART_LOG_DEBUG("func: %s has %d param(s)\n", func_name, param_count);
    }


    int pattern_idx = 0;
    ITER_EXPR_NEXT(patterns, cur, it)
        if (cur->type == ET_PATTERN) {
            errno = 0;
            _check_pattern_semantics(cc, module, func, cur, param_count, pattern_idx++);
            if (errno) { return; }
        }
    ITER_EXPR_END(it)
}

static void _to_postfix(klist_t(expr_t) * stack, expr_t * expr) {

    if (expr->left != NULL) {
        _to_postfix(stack, expr->left);
    }

    if (expr->right != NULL) {
        _to_postfix(stack, expr->right);
    }

    *kl_pushp(expr_t, stack) = expr;
}

static void _check_pattern_semantics(compile_context_t * cc, 
        module_t * module, 
        func_t * func, 
        expr_t * pattern, 
        int param_count, 
        int pattern_idx) {

    NEART_LOG_TRACE();
    expr_t * bindings = pattern->detail;
    expr_t * expr = bindings->next;
    bindings->next = NULL; // unhinge -> free of syntax tree should not free the expressions
    klist_t(expr_t) * postfix = kl_init(expr_t);

    // override the context within this pattern
    sym_table_t * table = neart_sym_table_push(cc->symbols);
    cc->symbols = table;


    int bindingCount = 0;
    if (bindings != NULL) {
        ITER_EXPR_NEXT(bindings->detail, binding, it)
            bindingCount++;
        ITER_EXPR_END(it)

        NEART_LOG_DEBUG("func %s pattern has %d binding(s)\n", func->name, bindingCount);
    }

    /*if (bindingCount != (paramCount - 1)) {
        errno = 1;
        NEART_LOG(LOG_FATAL, "in func '%s' pattern nmr %d has %d bindings but should have %d \n", func->name, pattern_idx+1, bindingCount, (paramCount - 1));
        goto bail_out_pat;
    }*/

    _to_postfix(postfix, expr);

    pattern_t * pat = neart_pattern_alloc(postfix);

    errno = 0;
    neart_func_add_pattern(func,pat);
    if (errno) { goto bail_out_pat; }

    kliter_t(expr_t) * it;

    NEART_LOG_DEBUG("%s(%d) postfix: ", func->name, pattern_idx);
    for (it = kl_begin(postfix); it != kl_end(postfix); it = kl_next(it)) {
        expr_t * e = kl_val(it);
        if (e->data == NULL) {
            NEART_LOG_DEBUG("%s ", expr_type_names[e->type]);
        } else {
            NEART_LOG_DEBUG("%s(%s) ", expr_type_names[e->type], e->data);
        }

    }
    NEART_LOG_DEBUG("\n");

    return;
bail_out_pat:
    kl_destroy(expr_t, postfix);
    bindings->next = expr;
}

