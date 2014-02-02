
#include "structs.h"

#include <stdio.h>
#include <stdlib.h>

#define ALLOC_STRUCT(structure,var) \
   structure * var = malloc(sizeof(structure)); \
   if (var == NULL) {               \
       perror("out of memory\n");   \
       exit(1);                     \
   }                                \

expr_t * expr_alloc(expr_type_t type) {
    ALLOC_STRUCT(expr_t, tree);
    tree->type = type;
    tree->left = NULL;
    tree->right = NULL;
    tree->leaf = NULL;
    return tree;
}

void expr_free(expr_t * tree) {
    free(tree);
}

void expr_free_r(expr_t * tree) {
    if (tree->left != NULL) {
        expr_free_r(tree->left);
    }
    if (tree->right != NULL) {
        expr_free_r(tree->right);
    }
    expr_free(tree);
}

//////////////////////////////////////// pattern
//
pattern_t * pattern_alloc(void) {
    ALLOC_STRUCT(pattern_t, pat);
    pat->bindings = kl_init(expr_t);
    return pat;
}

void pattern_free(pattern_t * pattern) {
    kl_destroy(expr_t, pattern->bindings);
    expr_free_r(pattern->expr);
    free(pattern);
    pattern = NULL;

}

void pattern_add_binding(pattern_t * pattern, expr_t * expr) {
    *kl_pushp(expr_t, pattern->bindings) = expr;
}

//////////////////////////////////////// func

func_t * func_alloc(const char * name) {
    ALLOC_STRUCT(func_t, func);
    func->name = name;
    func->params = kl_init(expr_t);
    func->patterns = kl_init(pattern_t);
    return func;
}

//////////////////////////////////////// context

context_t * context_alloc(void) {
    ALLOC_STRUCT(context_t, ctx);
    ctx->func_table = kh_init(str_func_t);
    ctx->last_func = NULL;
    return ctx;
}

void context_free(context_t * ctx) {
    kh_destroy(str_func_t, ctx->func_table);
    free(ctx);
    ctx = NULL;
}

void context_add_function(context_t * ctx, func_t * func) {
    int ret;
    khint_t k;

    ctx->last_func = func;
    k = kh_put(str_func_t, ctx->func_table, func->name, &ret);
    kh_value(ctx->func_table, k) = func;
}
