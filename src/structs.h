
#ifndef _STRUCTS_H_
#define _STRUCTS_H_

typedef enum expr_tree_type_t {
    TT_UNIT,
    TT_FUNC,
    TT_PATS,
    TT_LET,
    TT_ADD,
    TT_SUB,
    TT_MUL,
    TT_DIV,
    TT_LEAF
} expr_tree_type_t;

typedef struct expr_tree_t {
    struct expr_tree_t * left;
    struct expr_tree_t * right;
    expr_tree_type_t type;
    char * leaf;
} expr_tree_t;


expr_tree_t * expr_tree_alloc(expr_tree_type_t type);

/**
 * free the tree recursively
 */
void expr_tree_free_r(expr_tree_t * tree);

/**
 * free one node
 */
void expr_tree_free(expr_tree_t * tree);

typedef struct context_t {
    expr_tree_t * root;
} context_t;

void context_add_function(context_t * ctx, expr_tree_t * tree);

#endif
