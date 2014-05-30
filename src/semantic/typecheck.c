
#include "typecheck.h"

#include <errno.h>
#include "error.h"
#include "utils.h"
#include "logging.h"
#include "gc.h"

param_t INTEGER_PARAM[] = { type_int, 0x0, ',', 0x0 };
param_t BOOLEAN_PARAM[] = { type_bool, 0x0, ',', 0x0 };

typedef struct __pf_trans {
    compile_context_t * cc;
    expr_t * expr;
    param_t * expected_param;
} _pf_trans_t;

inline static sem_expr_t * _alloc_sem_expr(type_t type, expr_t * expr) {
    GC_ALLOC_STRUCT(sem_expr_t, spe);
    spe->next = spe->prev = NULL;
    spe->type = type;
    spe->lang_construct = construct_param;
    spe->type_specific = 0;
    spe->func = NULL;
    spe->expr = expr;
    spe->symbol_type = -1;
    spe->argument_index = -1;
    spe->assigned_register = -1;
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

    param_t * e = expected;
    param_t * a = actual;

    if (a == NULL || e == NULL) {
        return 0;
    }

    while (1) {

        if (neart_param_type(e) != neart_param_type(a)) {
            errno = ERR_TYPE_MISMATCH;
            neart_fatal_error(ERR_TYPE_MISMATCH, expr, 
                    "exected type '%s' but got '%s'\n", neart_type_name(*e), neart_type_name(*a) );
            return 0;
        }

        if (neart_param_type(e) == ',') {
            break;
        }

        e = neart_param_next(e);
        a = neart_param_next(a);
    }

    return 1;
}


static sem_expr_t * _type_check_func(_pf_trans_t * ctx, func_t * func) {

    int param_count;
    int has_next = 0;
    klist_t(expr_t) * list;
    params_t * params;
    expr_t * expr = ctx->expr;
    compile_context_t * cc = ctx->cc;
    sem_expr_t * spe = NULL;

    param_count = neart_params_count(func->params);
    params = func->params;
    list = _next_n_parameter(expr, param_count, &has_next);

    if (has_next) {
        neart_fatal_error(ERR_TYPE_TOO_MANY_ARGUMENTS, expr, 
                "the function '%s' has %d argument(s) but %ld is/are given.\n", func->name, param_count, has_next + list->size);
        kl_destroy(expr_t, list);
        return NULL;
    }

    sem_expr_t * ret_spe = _alloc_sem_expr(type_func, expr);
    ret_spe->func = func;
    ret_spe->apply = 1;
    ret_spe->lang_construct = construct_func;

    spe = ret_spe;

    int i = 0;
    kliter_t(expr_t) * it;
    for (it = kl_begin(list); it != kl_end(list); it = kl_next(it), i++) {
        expr_t * cur = kl_val(it);
        param_t * param = neart_param_at(params, i, 0);
        sem_expr_t * expr = neart_type_check(cc, cur, param);
        if (expr == NULL) {
            kl_destroy(expr_t, list);
            return NULL;
        }

        sem_expr_t * parens = _alloc_sem_expr(type_none, NULL);
        parens->lang_construct = construct_nest;
        parens->detail = expr;
        parens->argument_index = i;
        expr->argument_index = i;

        expr = parens;

        spe->next = expr;
        expr->prev = spe;
        spe = expr;
    }

    return ret_spe;
}

/**
 * Type checking returns a sem_expr_t if the type check succeeds.
 *
 */
sem_expr_t * _neart_type_check(_pf_trans_t * ctx) {
    sem_expr_t * spe = NULL;
    expr_t * expr = ctx->expr;
    compile_context_t * cc = ctx->cc;
    param_t * expected_result = ctx->expected_param;

    // don't be confused with parens. this only means that
    // on the root level this one is nested
    if (expr->type == ET_PARENS) {

        sem_expr_t * se = neart_type_check(cc, expr->left, expected_result);
        sem_expr_t * parens = _alloc_sem_expr(type_none, NULL);
        parens->detail = se;
        parens->lang_construct = construct_nest;
        return parens;
    }

    if (expr->type == ET_INTEGER) {
        if (!neart_type_match(expr, expected_result, INTEGER_PARAM)) {
            goto bail_out_type_check;
        }

        spe = _alloc_sem_expr(type_int, expr);
        return spe;
    }

    // either a function or a variable
    if (expr->type == ET_VARIABLE) {
        const char * name = expr->data;
        sym_entry_t * symbol = neart_sym_table_lookup(cc->symbols, name);
        if (symbol == NULL) {
            neart_fatal_error(ERR_SYM_NOT_FOUND, expr, "symbol '%s' not found\n", name );
            errno = ERR_SYM_NOT_FOUND;
            goto bail_out_type_check;
        }

        if (sym_entry_is(symbol, SYM_VAR)) {
            param_t * param = symbol->param;
            if (!neart_type_match(expr, expected_result, param)) {
                goto bail_out_type_check;
            }

            spe = _alloc_sem_expr(type_generic, expr);
            spe->type_specific = symbol->type;
            spe->symbol_type = symbol->entry_type;
            spe->argument_index = symbol->argument_index;
            return spe;
        } else if (sym_entry_is(symbol, SYM_FUNC)) {
            return _type_check_func(ctx, symbol->func);
        } else {
            printf("XXX unkown symbol entry type\n");
            goto bail_out_type_check;
        }
    }


    // look at the layout in ast.h
    if (expr->type == ET_IF) {
        sem_expr_t * cond_expr = neart_type_check(cc, expr->left, BOOLEAN_PARAM);
        if (cond_expr == NULL) {
            goto bail_out_type_check;
        }
        sem_expr_t * then_expr = neart_type_check(cc, expr->right, expected_result);
        if (then_expr == NULL) {
            goto bail_out_type_check;
        }
        sem_expr_t * else_expr = neart_type_check(cc, expr->right->right, expected_result);
        if (else_expr == NULL) {
            goto bail_out_type_check;
        }
        // chain them
        then_expr->next = else_expr;
        else_expr->prev = then_expr;

        // if is not a type -> it is a construct of bool,expr,expr
        sem_expr_t * _if = _alloc_sem_expr(type_none, expr);
        _if->detail = cond_expr;
        _if->lang_construct = construct_if;
        // chain them
        _if->detail = cond_expr;
        _if->next = then_expr;
        then_expr->prev = _if;

        if (expr->right->right->right != NULL) {
            IMPL_ME();
            goto bail_out_type_check;
        }

        return _if;
    }
    if (expr->type == ET_THEN) {
        sem_expr_t * then_expr = neart_type_check(cc, expr->left, expected_result);
        if (then_expr == NULL) {
            goto bail_out_type_check;
        }
        sem_expr_t * then = _alloc_sem_expr(type_none, expr);
        then->detail = then_expr;
        then->lang_construct = construct_then;
        return then;
    }
    if (expr->type == ET_ELSE) {
        sem_expr_t * _else_expr = neart_type_check(cc, expr->left, expected_result);
        if (_else_expr == NULL) {
            goto bail_out_type_check;
        }
        sem_expr_t * _else = _alloc_sem_expr(type_none, expr);
        _else->detail = _else_expr;
        _else->lang_construct = construct_else;
        return _else;
    }


    func_t * builtin = neart_builtin_func_lookup(expr->type);
    if (builtin != NULL) {

        param_t * func_ret_type = neart_params_last(builtin->params);

        if (!neart_type_match(expr, expected_result, func_ret_type)) {
            goto bail_out_type_check;
        }

        sem_expr_t * spe = _type_check_func(ctx, builtin);
        spe->type = type_func_builtin;
        if (spe == NULL) {
            goto bail_out_type_check;
        }
        return spe;
    }

bail_out_type_check:

    printf("BAIL OUT\n");
    return NULL;
}

/**
 * This function checks if the given syntax conform the static type system.
 * There is an expected result.
 */
sem_expr_t * neart_type_check(compile_context_t * cc, expr_t * expr, param_t * expected_result) {
    _pf_trans_t pf;
    pf.cc = cc;
    pf.expr = expr;
    pf.expected_param = expected_result;

    // TODO check binding types

    return _neart_type_check(&pf);
}
