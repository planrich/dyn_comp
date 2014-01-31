
#include "structs.h"

#include <stdio.h>
#include <stdlib.h>

expr_tree_t * expr_tree_alloc(expr_tree_type_t type) {
    expr_tree_t * tree = calloc(sizeof(expr_tree_t),1);
    if (tree == NULL) {
        fprintf(stderr, "to little space to alloc expr tree\n");
        exit(1);
    }
    tree->type = type;
    return tree;
}

void expr_tree_free(expr_tree_t * tree) {
    free(tree);
}

void expr_tree_free_r(expr_tree_t * tree) {
    if (tree->left != NULL) {
        expr_tree_free_r(tree->left);
    }
    if (tree->right != NULL) {
        expr_tree_free_r(tree->right);
    }
    expr_tree_free(tree);
}
