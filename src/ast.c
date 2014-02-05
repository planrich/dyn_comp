
#include "ast.h"

#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

// IDEA alloc sizeof(expr_t * x) -> and dyn inc x if space it too little
expr_t * neart_expr_alloc(expr_type_t type) {
    ALLOC_STRUCT(expr_t, tree);
    tree->type = type;
    tree->next = NULL;
    tree->detail = NULL;
    tree->data = NULL;
    return tree;
}

void neart_expr_free(expr_t * tree) {
    if (tree->data != NULL) {
        free(tree->data);
    }
    free(tree);
}

void neart_expr_free_r(expr_t * tree) {
    if (tree->left != NULL) {
        neart_expr_free_r(tree->left);
    }
    if (tree->right != NULL) {
        neart_expr_free_r(tree->right);
    }
    neart_expr_free(tree);
}

