
#include "typecheck.h"

#include <errno.h>
#include "error.h"
#include "utils.h"
#include "logging.h"

typedef struct __pf_trans {
    sym_table_t * symbols;
    sem_post_expr_t * cur;
    sem_post_expr_t ** head;
    int params_to_consume;
} _pf_trans_t;

/*
static int _type_check_func(const char * name, params_t * params, klist_t(sem_post_expr_t) * stack) {

    int i;
    int param_count = (*params) - 1;

    if (stack->size < param_count) {
        NEART_LOG_FATAL("func %s needs %d parameter but only %ld are given!\n", name, param_count, stack->size);
        return ERR_TYPE_TO_FEW_PARAMETER;
    }

    param_t * param = NULL;

    
    for (i = 0; i < param_count; i++) {
        param = neart_param_at(params, i, 0);
        sem_post_expr_t top = kl_val(kl_begin(stack));
        kl_shift(sem_post_expr_t, stack, 0);

        if (top.type != neart_param_type(param)) {
            *kl_pushp(sem_post_expr_t, stack) = top;
            return ERR_TYPE_MISMATCH;
        }
    }

    return 0;
}
*/

static klist_t(expr_t) * _next_n_parameter(expr_t * expr, int n, int * has_next) {
    klist_t(expr_t) * list = kl_init(expr_t);
    expr_t * cursor = expr;

    // binary ops might habe their first param on the left...
    // e.g. +, -, ...
    if (n == 2 && cursor->left != NULL) {
        NLH
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
        NLH
    }

    *has_next = 0;
    if (cursor->right != NULL) {
        *has_next = 1;
    }

    return list;
}

static sem_post_expr_t * _type_check_func(compile_context_t * cc, func_t * func, expr_t * expr) {

    int param_count;
    int has_next = 0;
    klist_t(expr_t) * list;
    params_t * params;

    param_count = neart_params_count(func->params) - 1;
    params = func->params;
    printf("param count %d\n", param_count);
    list = _next_n_parameter(expr, param_count, &has_next);

    if (has_next) {
        printf("has too many parameters\n");
        kl_destroy(expr_t, list);
        return NULL;
    }

    int i = 0;
    kliter_t(expr_t) * it;
    for (it = kl_begin(list); it != kl_end(list); it = kl_next(it), i++) {
        expr_t * cur = kl_val(it);
        param_t * param = neart_param_at(params, i, 0);
        sem_post_expr_t * expr = neart_type_check(cc, expr, param);
    }

    /*
    klist_t(expr_t) * stack = kl_init(expr_t);
    *kl_pushp(expr_t, stack) = expr;
    expr_t * cur;
    sym_table_t * symt = cc->symbols;

    while (stack->size > 0) {
        kl_shift(expr_t, stack, &cur);


        if ( cur->type == ET_VARIABLE ) {
            const char * name = cur->data;

            sym_entry_t * symbol = neart_sym_table_lookup(symt, name);
            if (symbol == NULL) {
                printf("couldn't find %s in the context!\n", name);
                errno = ERR_SYM_NOT_FOUND;
                goto bail_out_type_check;
            }
            printf("%s found in ctx creating sem post expr node\n", name);

            if (symbol->type == type_func) {
                int param_count = neart_params_count(symbol->func->params);
                printf("%s has %d params\n", symbol->func->name, param_count);
            } else {

            }
            ALLOC_STRUCT(sem_post_expr_t, s);
            s->expr = expr;
            s->type = symbol->type;
            s->entry = symbol;
            s->next = NULL;
            s->prev = spe;
            spe->next = s;
            spe = s;
        }

        if (cur->right != NULL) {
            *kl_fpushp(expr_t, stack) = cur->right;
        }
        if (cur->left != NULL) {
            *kl_fpushp(expr_t, stack) = cur->left;
        }

    }*/

    return NULL;
}

int neart_type_match(param_t * expected, param_t * actual) {

    while (1) {

        if (neart_param_type(expected) != neart_param_type(actual)) {
            printf("expected %c actual %c\n", *expected, *actual);
            return 0;
        }
        printf("match %c!\n", *expected);

        if (neart_param_type(expected) == ',') {
            break;
        }

        expected = neart_param_next(expected);
        actual = neart_param_next(actual);
    }

    return 1;
}

sem_post_expr_t * neart_type_check(compile_context_t * cc, expr_t * expr, param_t * expected_result) {

    if (expr->type == ET_INTEGER) {
        param_t integer_param[] = { type_int, 0x0, ',', 0x0 };
        if (!neart_type_match(expected_result, integer_param)) {
            goto bail_out_type_check;
        }
        if (expr->left != NULL) { goto bail_out_type_check; }
        if (expr->right != NULL) { goto bail_out_type_check; }
        return NULL;
    }

    if (expr->type == ET_VARIABLE) {
        sym_entry_t * symbol = neart_sym_table_lookup(cc->symbols, expr->data);
        if (symbol == NULL) {
            printf("couldn't find %s in the context!\n", expr->data);
            errno = ERR_SYM_NOT_FOUND;
            goto bail_out_type_check;
        }

        if (symbol->entry_type == SYM_VAR) {
            param_t * param = symbol->param;
            if (!neart_type_match(expected_result, param)) {
                goto bail_out_type_check;
            }
            if (expr->left != NULL) { goto bail_out_type_check; }
            if (expr->right != NULL) { goto bail_out_type_check; }
            return NULL;
        } else {
            printf("unkown symbol entry type\n");
            goto bail_out_type_check;
        }
    }

    func_t * builtin = neart_builtin_func_lookup(expr->type);
    if (builtin != NULL) {

        printf("matching bi %c\n", builtin->name);
        param_t * func_ret_type = neart_params_last(builtin->params);
        if (!neart_type_match(expected_result, func_ret_type)) {
            goto bail_out_type_check;
        }

        sem_post_expr_t * spe = _type_check_func(cc, builtin, builtin);
        if (spe == NULL) {
            goto bail_out_type_check;
        }
        return spe;
    }

    goto bail_out_type_check;

    //_to_postfix(NULL, expr, &spe);
    /*
    sym_table_t * symt = cc->symbols;

    sem_post_expr_t * cur = spe;
    const char * name;

    while (cur != NULL) {

        switch ( cur->expr->type ) {
            case ET_VARIABLE:
                name = cur->expr->data;

                sym_entry_t * found = neart_sym_table_lookup(symt, name);
                if (found == NULL) {
                    printf("couldn't find %s in the context!\n", name);
                    errno = ERR_SYM_NOT_FOUND;
                    goto bail_out_type_check;
                }
                printf("%s found in ctx\n", name);

                cur->entry = found;

                cur->type = found->type;
                if (found->entry_type == SYM_VAR) {
                    // push expr on the stack
                    *kl_pushp(sem_post_expr_t, stack) = *cur;
                } else if (found->entry_type == SYM_FUNC) {
                    // reduce a function
                    func_t * func = found->func;
                    params_t * params = func->params;
                    printf("reducing func %s\n", func->name);

                    errno = 0; //_type_check_func(found->func->name, params, stack);
                    if (errno) {
                        *kl_pushp(sem_post_expr_t, stack) = *cur;
                    } else {
                        sem_post_expr_t sem;
                        sem.type = neart_param_type(neart_param_last(params));
                        sem.expr = NULL;
                        sem.entry = NULL;

                        *kl_pushp(sem_post_expr_t, stack) = sem;
                    }

                } else if (found->entry_type == SYM_ANON_FUNC) {
                    // reduce an anon function
                    param_t * param = found->param;
                    printf("reducing anon func %x\n", param);
                    params_t * anon_params = neart_params_anon_func(param);
                    if (anon_params == NULL) {
                        errno = ERR_INTERNAL_TRANSFORM_ANON_FUNC;
                        goto bail_out_type_check;
                    }

                    errno = 0; //_type_check_func("anonymous", anon_params, stack);

                    if (errno) {
                        // anon function might reduce to fast! try again later
                        printf("did not work out -> try again\n");
                        *kl_pushp(sem_post_expr_t, stack) = *cur;
                    } else {
                        sem_post_expr_t sem;
                        sem.type = neart_param_type(neart_param_last(anon_params));
                        sem.expr = NULL;
                        sem.entry = NULL;
                        *kl_pushp(sem_post_expr_t, stack) = sem;
                    }

                    free(anon_params);
                } else {
                    errno = ERR_SYM_ENTRY_UNKOWN;
                    goto bail_out_type_check;
                }
                
                break;
            case ET_OP_IADD:
            case ET_OP_ISUB:
            case ET_OP_IMUL:
            case ET_OP_IDIV:

                break;
            case ET_OP_CONS:
                break;

            case ET_INTEGER:
                cur->type = type_int32;
                *kl_pushp(sem_post_expr_t, stack) = *cur;
                break;
            default:
                NEART_LOG_DEBUG("unhandled %s\n", expr_type_names[cur->expr->type]);
        }


        cur = cur->next;
    }

    kl_destroy(sem_post_expr_t, stack);

    cur = spe;

    NEART_LOG_DEBUG("infix: ");
    while (cur != NULL) {
        expr_t * e = cur->expr;
        if (e->data == NULL) {
            NEART_LOG_DEBUG("%s ", expr_type_names[e->type]);
        } else {
            NEART_LOG_DEBUG("%s(%s) ", expr_type_names[e->type], e->data);
        }

        cur = cur->next;
    }
    NEART_LOG_DEBUG("\n");

    return spe;
    */
bail_out_type_check:

    //neart_sem_post_expr_free(spe);
    //kl_destroy(sem_post_expr_t, stack);
    printf("TYPE MATCH FAILED\n");

    return NULL;
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

