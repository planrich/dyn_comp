
#include "dot_syntax_tree.h"

#include <stdio.h>

int _write_node(FILE * file, int id, expr_t * tree) {

    if (tree == NULL) {
        return id;
    }

    int root = id++;

    printf("tree %d\n", expr_type_names[tree->type]);
    fprintf(file, " n%d [ label=%s ]; \n", root, expr_type_names[tree->type]);
    if (tree->detail != NULL) {
        int detail = _write_node(file, id, tree->detail);
        fprintf(file, " n%d -> n%d; \n", root, detail);
    }
    if (tree->next != NULL) {
        int next = _write_node(file, id, tree->next);
        fprintf(file, " n%d -> n%d; \n", root, next);
    }

    return id;
}


void _write_tree(FILE * file, expr_t * tree) {

    fprintf(file, "digraph syntax_tree {\n");
    fprintf(file, " null [ label=null ]; \n");

    _write_node(file, 0, tree);

    fprintf(file, "}\n");
}

void neart_write_syntax_tree_to_file(const char * filename, expr_t * tree) {
    FILE * file = fopen(filename, "w");

    _write_tree(file, tree);

    fclose(file);
}
