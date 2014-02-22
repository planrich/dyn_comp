
#include "ast.h"

#include <stdio.h>
#include <stdlib.h>
#include "utils.h"
#include "logging.h"


const char * expr_type_names[] = {
    FOREACH_EXPR_TYPE(ENUM_HNAME)
};

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

#include "logging.h"

int32_t neart_expr_to_int32(expr_t * expr) {

    int32_t out;

    char * end_ptr = NULL;
    out = strtol(expr->data, &end_ptr, 10);

    if (*end_ptr != '\0') {
        NEART_LOG_FATAL("could not parse integer '%s'. returned -1\n", expr->data);
        return -1;
    }

    return out;
}
