
#include "gpir.h"

//////////////////////////////////////// context

/*
context_t * neart_context_alloc(const char * filename) {
    ALLOC_STRUCT(context_t, ctx);
    ctx->func_table = kh_init(str_func_t);
    ctx->syntax_tree = NULL;
    ctx->filename = filename;
    return ctx;
}

void neart_context_free(context_t * ctx) {
    kh_destroy(str_func_t, ctx->func_table);
    neart_expr_free_r(ctx->syntax_tree);
    free(ctx);
    ctx = NULL;
}

void neart_context_add_function(context_t * ctx, func_t * func) {
    int ret;
    khint_t k;

    k = kh_put(str_func_t, ctx->func_table, func->name, &ret);
    kh_value(ctx->func_table, k) = func;
}*/

//////////////////////////////////////// pattern

/*
pattern_t * pattern_alloc(void) {
    ALLOC_STRUCT(pattern_t, pat);
    pat->bindings = kl_init(expr_t);
    return pat;
}

void pattern_free(pattern_t * pattern) {
    kl_destroy(expr_t, pattern->bindings);
    neart_expr_free_r(pattern->expr);
    free(pattern);
    pattern = NULL;

}

void pattern_add_binding(pattern_t * pattern, expr_t * expr) {
    *kl_pushp(expr_t, pattern->bindings) = expr;
}*/

//////////////////////////////////////// func

/*
func_t * func_alloc(const char * name) {
    ALLOC_STRUCT(func_t, func);
    func->name = name;
    func->params = kl_init(expr_t);
    func->patterns = kl_init(pattern_t);
    return func;
}*/
