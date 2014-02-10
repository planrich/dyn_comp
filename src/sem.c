
#include "sem.h"

#include <stdio.h>
#include <errno.h>
#include "klist.h"
#include "utils.h"
#include "logging.h"
#include "types.h"

const char * BINDING_WILDCARD_STR = "_";

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

    const char * func_name = func->name;
    expr_t * params_expr = expr->detail;
    expr_t * patterns = params_expr->next;

    int param_count = 0;
    if (params_expr != NULL) {
        func->params = neart_params_transform(module, params_expr->detail, &param_count);
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

static void _declare_bindings(compile_context_t * cc, params_t * params, expr_t * binding, int param_index, int depth) {

    if (binding == NULL) {
        return;
    }

    const char * name = binding->data;
    if (binding->type == ET_VARIABLE) {
        if (strcmp(name, BINDING_WILDCARD_STR) == 0) {
            return;
        }

        param_t * param = neart_param_at(params, param_index, depth);
        if (param == NULL) {
            NEART_LOG_FATAL("parameter %d at nesting %d not available\n", param_index, depth);
        }
        type_t type = neart_param_type(param);
        NEART_LOG_DEBUG("binding defining %s as %c\n", name, type);
        // consider this? can one unpack a function (a -> (b -> c)) unpack (a:bc)?
        param_t * top_param = neart_param_at(params, param_index, 0);
        if (neart_param_type(top_param) == type_func && depth > 0) {
            NEART_LOG_FATAL("tried to unpack func as you do with lists!\n");
        }

        sym_table_t * table = cc->symbols;
        sym_entry_t entry;
        entry.type = type;
        entry.entry_type = SYM_VAR;

        entry.var = binding;

        neart_sym_table_insert(table, name, entry);
    }

    _declare_bindings(cc, params, binding->left, param_index, depth + ((binding->type == ET_CONS) ? 1 : 0));
    _declare_bindings(cc, params, binding->right, param_index, depth);
}
/**
 * bindings can are nested/wrapped by [] or (_:_)
 */
static void _declare_all_bindings(compile_context_t * cc, params_t * params, expr_t * binding, int * binding_count) {
    int param_index = 0;
    expr_t * next = binding;
    expr_t * tmp;
    while (next != NULL) {
        tmp = next->next;
        next->next = NULL; // unhinge to not recurse in _declare_bindings

        _declare_bindings(cc, params, next, param_index++, 0);

        next->next = tmp;
        next = tmp;
    }
    *binding_count = param_index;
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
    klist_t(expr_t) * postfix = kl_init(expr_t);

    // override the context within this pattern
    sym_table_t * table = neart_sym_table_push(cc->symbols);
    func->symbols = table;
    cc->symbols = table;

    _to_postfix(postfix, expr);

    pattern_t * pat = neart_pattern_alloc(postfix);

    // bindings
    int binding_count = 0;
    if (bindings != NULL) {
        _declare_all_bindings(cc, func->params, bindings->detail, &binding_count);
        pat->bindings = bindings->detail;
        bindings->detail = NULL; // unhinge to prevent free of ast

        NEART_LOG_DEBUG("func %s pattern has %d binding(s)\n", func->name, binding_count);
    }

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

    cc->symbols = neart_sym_table_pop(cc->symbols, 0);

    return;
bail_out_pat:
    kl_destroy(expr_t, postfix);
    bindings->next = expr;
}

