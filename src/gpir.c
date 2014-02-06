
#include "gpir.h"

#include "utils.h"
#include "kstring.h"

//////////////////////////////////////// module


module_t * neart_module_alloc(const char * name) {
    ALLOC_STRUCT(module_t, m);
    m->func_table = kh_init(str_func_t);
    m->name = strdup(name);
    return m;
}

void neart_module_free(module_t * mod) {
    kh_destroy(str_func_t, mod->func_table);
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
    kl_destroy(expr_t, pattern->expr_postfix);
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
    free((void*)func->name);
    kl_destroy(expr_t,func->params);
    kl_destroy(pattern_t,func->patterns);
    free(func);
}

inline
void neart_func_add_pattern(func_t * func, pattern_t * pattern) {
    *kl_pushp(pattern_t, func->patterns) = pattern;
}


void neart_module_add_function(compile_context_t * cc, module_t * ctx, func_t * func) {


/*
void neart_context_add_function(context_t * ctx, func_t * func) {
    int ret;
    khint_t k;

    k = kh_put(str_func_t, ctx->func_table, func->name, &ret);
    kh_value(ctx->func_table, k) = func;
}*/
}
