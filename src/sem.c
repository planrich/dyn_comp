
#include "sem.h"

#include <stdio.h>
#include <errno.h>
#include "utils.h"
#include "logging.h"
#include "klist.h"

#define ITERATE_EXPR(node, current, block) \
    {                      \
        expr_t * it = node;   \
        while (it != NULL) {  \
            expr_t * current = it; \
            block             \
            it = it->next;    \
        }                     \
    }

#define FOREACH_BUILTIN_TYPE(PBT) \
    PBT(bi_int8,    "int8"), \
    PBT(bi_int16,   "int16"), \
    PBT(bi_int32,   "int32"), \
    PBT(bi_int,     "int"), \
    PBT(bi_int64,   "int64"), \
    PBT(bi_str,     "str"), \
    PBT(bi_bin,     "bin"), \
    PBT(bi_list,    "list"), \
    PBT(bi_func,    "func"), \

#define PULL_BI_PARAM1(p1, p2, ...) p1


typedef enum builtin_types_t {
    FOREACH_BUILTIN_TYPE(PULL_BI_PARAM1)
} builtin_types_t;

void _check_func_semantics(compile_context_t * cc, module_t * module, expr_t * func);
void _check_pattern_semantics(compile_context_t * cc,
        module_t * module,
        func_t * func,
        expr_t * pattern,
        int paramCount,
        int patternIdx);

module_t * neart_check_semantics(compile_context_t * cc, expr_t * root) {
    NEART_LOG_TRACE();

    module_t * module = neart_module_alloc(root->data);

    ITERATE_EXPR(root->detail, cur, {

        if (cur->type == ET_FUNC) {
            errno = 0;
            _check_func_semantics(cc, module, cur);
            if (errno) { return NULL; }
        }

    })

    return module;
}

void _check_func_semantics(compile_context_t * cc, module_t * module, expr_t * func) {
    NEART_LOG_TRACE();

    const char * func_name = func->data;
    expr_t * params = func->detail;
    expr_t * patterns = params->next;

    func_t * function = neart_func_alloc(func_name);

    int paramCount = -1;
    if (params != NULL) {
        paramCount = 0;
        ITERATE_EXPR(params->detail, param, {
            paramCount++;
        })
        NEART_LOG_DEBUG("func: %s has %d param(s)\n", func_name, paramCount);
    }

    int pattern_idx = 0;
    ITERATE_EXPR(patterns, cur, {

        if (cur->type == ET_PATTERN) {
            errno = 0;
            _check_pattern_semantics(cc, module, function, cur, paramCount, pattern_idx++);
            if (errno) { goto bail_out_func; }
            //_check_func_semantics(cc, module, cur);
            //if (errno) { return NULL; }
        }

    })

    return;
bail_out_func:
    neart_func_free(function);

}

void _to_postfix(klist_t(expr_t) * stack, expr_t * expr) {

    if (expr->left != NULL) {
        _to_postfix(stack, expr->left);
    }

    if (expr->right != NULL) {
        _to_postfix(stack, expr->right);
    }

    *kl_pushp(expr_t, stack) = expr;
}

void _check_pattern_semantics(compile_context_t * cc, module_t * module, func_t * func, expr_t * pattern, int paramCount, int pattern_idx) {
    NEART_LOG_TRACE();
    expr_t * bindings = pattern->detail;
    expr_t * expr = bindings->next;
    bindings->next = NULL; // unhinge -> free of syntax tree should not free the expressions
    klist_t(expr_t) * postfix = kl_init(expr_t);

    int bindingCount = 0;
    if (bindings != NULL) {
        ITERATE_EXPR(bindings->detail, binding, {
            bindingCount++;
        })
        NEART_LOG_DEBUG("func %s pattern has %d binding(s)\n", func->name, bindingCount);
    }

    /*if (bindingCount != (paramCount - 1)) {
        errno = 1;
        NEART_LOG(LOG_FATAL, "in func '%s' pattern nmr %d has %d bindings but should have %d \n", func->name, pattern_idx+1, bindingCount, (paramCount - 1));
        goto bail_out_pat;
    }*/

    _to_postfix(postfix, expr);

    pattern_t * pat = neart_pattern_alloc(postfix);

    neart_func_add_pattern(func, pat);

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
    errno = 1;
    kl_destroy(expr_t, postfix);
    bindings->next = expr;
}
