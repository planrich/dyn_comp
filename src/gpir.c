
#include "gpir.h"

#include <errno.h>
#include "utils.h"
#include "kstring.h"
#include "sem.h"
#include "logging.h"
#include "ast.h"


//////////////////////////////////////// module


module_t * neart_module_alloc(const char * name) {
    ALLOC_STRUCT(module_t, m);
    m->func_table = kh_init(str_func_t);
    m->symbols = neart_sym_table_alloc();
    m->name = strdup(name);
    return m;
}

void neart_module_free(module_t * mod) {
    NEART_LOG_TRACE();
    khiter_t k;
    khash_t(str_func_t) * h = mod->func_table;

    for (k = kh_begin(h) ; k < kh_end(h); ++k) {
        if (kh_exist(h, k)) {
            func_t * f = kh_value(h, k);
            neart_func_free(f);
            kh_value(h, k) = NULL;
        }
    }
    kh_destroy(str_func_t, h);
    neart_sym_table_free(mod->symbols);
    free((void*)mod->name);
    free(mod);
}

//////////////////////////////////////// pattern

pattern_t * neart_pattern_alloc(klist_t(expr_t) * postfix) {
    ALLOC_STRUCT(pattern_t, pat);
    pat->expr_postfix = postfix;
    return pat;
}

void neart_pattern_free(pattern_t * pattern) {
    {
        kliter_t(expr_t) * it;
        klist_t(expr_t) * l = pattern->expr_postfix;

        for (it = kl_begin(l); it != kl_end(l); it = kl_next(it)) {
            expr_t * expr = kl_val(it);
            neart_expr_free(expr);
        }
        kl_destroy(expr_t, pattern->expr_postfix);
    }
    free(pattern);
}
/*

void pattern_add_binding(pattern_t * pattern, expr_t * expr) {
    *kl_pushp(expr_t, pattern->bindings) = expr;
}*/

//////////////////////////////////////// func

func_t * neart_func_alloc(const char * name) {

    ALLOC_STRUCT(func_t, f);

    f->name = strdup(name);
    f->params = kl_init(expr_t);
    f->patterns = kl_init(pattern_t);
    return f;
}

void neart_func_free(func_t * func) {

    {
        kliter_t(expr_t) * it;
        klist_t(expr_t) * l = func->params;

        for (it = kl_begin(l); it != kl_end(l); it = kl_next(it)) {
            expr_t * expr = kl_val(it);
            neart_expr_free(expr);
        }
        kl_destroy(expr_t,func->params);
    }

    free((void*)func->name);

    {
        kliter_t(pattern_t) * it;
        klist_t(pattern_t) * l = func->patterns;
        for (it = kl_begin(l); it != kl_end(l); it = kl_next(it)) {
            pattern_t * pattern = kl_val(it);
            neart_pattern_free(pattern);
        }

        kl_destroy(pattern_t,func->patterns);
    }
    free(func);
}

inline
void neart_func_add_pattern(func_t * func, pattern_t * pattern) {
    *kl_pushp(pattern_t, func->patterns) = pattern;
}


void neart_module_add_function(compile_context_t * cc, module_t * mod, func_t * func) {
    int ret;
    khint_t k;

    k = kh_put(str_func_t, mod->func_table, func->name, &ret);
    if (!ret) {
        errno = ERR_FUNC_ALREADY_DEF;
        NEART_LOG(LOG_FATAL, "function %s is already defined in modulde %s\n", func->name, mod->name);
    } else {
        kh_value(mod->func_table, k) = func;
    }
}
