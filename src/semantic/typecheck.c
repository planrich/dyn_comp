
#include "typecheck.h"

#include <errno.h>
#include "error.h"
#include "utils.h"
#include "logging.h"

typedef struct __pf_trans {
    compile_context_t * cc;
    expr_t * expr;
    param_t * expected_param;
} _pf_trans_t;

inline static sem_post_expr_t * _alloc_sem_post_expr(type_t type, expr_t * expr, sym_entry_t * entry) {
    ALLOC_STRUCT(sem_post_expr_t, spe);
    spe->next = spe->prev = NULL;
    spe->type = type;
    spe->expr = expr;
    spe->entry = entry;
    return spe;
}

static klist_t(expr_t) * _next_n_parameter(expr_t * expr, int n, int * has_next) {
    klist_t(expr_t) * list = kl_init(expr_t);
    expr_t * cursor = expr;

    // binary ops might habe their first param on the left...
    // e.g. +, -, ...
    if (n == 2 && cursor->left != NULL) {
        *kl_pushp(expr_t,list) = cursor->left;
        n--;
    }

    while (n > 0) {
        if (cursor->right == NULL) {
            kl_destroy(expr_t, list);
            return NULL;
        }

        *kl_pushp(expr_t,list) = cursor->right;
        cursor = cursor->right;
        n--;
    }

    *has_next = 0;
    while (cursor->right != NULL) {
        *has_next += 1;
        cursor = cursor->right;
    }

    return list;
}

static int neart_type_match(expr_t * expr, param_t * expected, param_t * actual) {

    while (1) {

        if (neart_param_type(expected) != neart_param_type(actual)) {
            errno = ERR_TYPE_MISMATCH;
            neart_fatal_error(ERR_TYPE_MISMATCH, expr, 
                    "exected type '%s' but got '%s'\n", neart_type_name(*expected), neart_type_name(*actual) );
            return 0;
        }

        if (neart_param_type(expected) == ',') {
            break;
        }

        expected = neart_param_next(expected);
        actual = neart_param_next(actual);
    }

    return 1;
}


static sem_post_expr_t * _type_check_func(_pf_trans_t * ctx, func_t * func) {

    int param_count;
    int has_next = 0;
    klist_t(expr_t) * list;
    params_t * params;
    expr_t * expr = ctx->expr;
    compile_context_t * cc = ctx->cc;
    sem_post_expr_t * spe = NULL;

    param_count = neart_params_count(func->params);
    params = func->params;
    list = _next_n_parameter(expr, param_count, &has_next);

    if (has_next) {
        neart_fatal_error(ERR_TYPE_TOO_MANY_ARGUMENTS, expr, 
                "the function '%s' has %d argument(s) but %d is/are given.\n", func->name, param_count, has_next + list->size);
        kl_destroy(expr_t, list);
        return NULL;
    }

    int i = 0;
    kliter_t(expr_t) * it;
    for (it = kl_begin(list); it != kl_end(list); it = kl_next(it), i++) {
        expr_t * cur = kl_val(it);
        param_t * param = neart_param_at(params, i, 0);
        sem_post_expr_t * expr = neart_type_check(cc, cur, param);
        if (spe != NULL) {
            spe->next = expr;
            expr->prev = spe;
        }
        spe = expr;
    }

    return spe;
}

sem_post_expr_t * _neart_type_check(_pf_trans_t * ctx) {
    sem_post_expr_t * spe = NULL;
    expr_t * expr = ctx->expr;
    compile_context_t * cc = ctx->cc;
    param_t * expected_result = ctx->expected_param;

    if (expr->type == ET_INTEGER) {
        param_t integer_param[] = { type_int, 0x0, ',', 0x0 };
        if (!neart_type_match(expr, expected_result, integer_param)) {
            goto bail_out_type_check;
        }
        if (expr->left != NULL) { goto bail_out_type_check; }
        if (expr->right != NULL) { goto bail_out_type_check; }

        return _alloc_sem_post_expr(type_int, expr, NULL);
    }

    if (expr->type == ET_VARIABLE) {
        const char * name = expr->data;
        sym_entry_t * symbol = neart_sym_table_lookup(cc->symbols, name);
        if (symbol == NULL) {
            neart_fatal_error(ERR_SYM_NOT_FOUND, expr, "symbol '%s' not found\n", name );
            errno = ERR_SYM_NOT_FOUND;
            goto bail_out_type_check;
        }

        if (symbol->entry_type == SYM_VAR) {
            param_t * param = symbol->param;
            if (!neart_type_match(expr, expected_result, param)) {
                goto bail_out_type_check;
            }
            if (expr->left != NULL) { goto bail_out_type_check; }
            if (expr->right != NULL) { goto bail_out_type_check; }

            return _alloc_sem_post_expr(type_generic, expr, symbol);
        } else {
            printf("XXX unkown symbol entry type\n");
            goto bail_out_type_check;
        }
    }

    func_t * builtin = neart_builtin_func_lookup(expr->type);
    if (builtin != NULL) {

        param_t * func_ret_type = neart_params_last(builtin->params);

        if (!neart_type_match(expr, expected_result, func_ret_type)) {
            goto bail_out_type_check;
        }

        sem_post_expr_t * spe = _type_check_func(ctx, builtin);
        if (spe == NULL) {
            goto bail_out_type_check;
        }
        sem_post_expr_t * func_apply = _alloc_sem_post_expr('$', expr, NULL);
        func_apply->prev = spe;
        spe->next = func_apply;
        return func_apply;
    }

    return spe;

bail_out_type_check:

    printf("TYPE MATCH FAILED\n");

    return NULL;

}

sem_post_expr_t * neart_type_check(compile_context_t * cc, expr_t * expr, param_t * expected_result) {

    _pf_trans_t pf;
    pf.cc = cc;
    pf.expr = expr;
    pf.expected_param = expected_result;

    return _neart_type_check(&pf);
}

void neart_sem_post_expr_free(sem_post_expr_t * expr) {

    sem_post_expr_t * cur = expr;
    sem_post_expr_t * next = cur->next;

    while (next != NULL) {
        free(cur);
        cur = next;
        next = next->next;
    }
}

    
