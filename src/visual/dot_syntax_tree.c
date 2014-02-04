
#include "dot_syntax_tree.h"

#include <stdio.h>
#include "logging.h"

int _write_node(FILE * file, int id, expr_t * tree) {

    if (tree == NULL) {
        return id;
    }

    int root = id++;
    int nullidnext = id++;
    int nulliddetail = id++;

    if (tree->type == ET_FUNC) { fprintf(file, "subgraph cluster_%d { label=\"func %s\"; \n", root, tree->data); }
    fprintf(file, " n%d [ label=\"%s %s\" ]; \n", root, expr_type_names[tree->type], tree->data == NULL ? "" : tree->data);

    if (tree->detail != NULL) {
        int detail = _write_node(file, id, tree->detail);
        fprintf(file, " n%d -> n%d; \n", root, id);
        id = detail;
    } else {
        //fprintf(file, " null%d [ shape=point ]; \n", nulliddetail);
        //fprintf(file, " n%d -> null%d; \n", root, nulliddetail);
    }
    if (tree->type == ET_FUNC) { fprintf(file, "}\n"); }

    if (tree->next != NULL) {
        int next = _write_node(file, id, tree->next);
        fprintf(file, " n%d -> n%d [ color=gray ]; \n", root, id);
        id = next;
    } else {
        fprintf(file, " null%d [ shape=point, color=gray ]; \n", nullidnext);
        fprintf(file, " n%d -> null%d [ color=gray ]; \n", root, nullidnext);
    }


    return id;
}


void _write_tree(FILE * file, expr_t * tree) {

    fprintf(file, "digraph syntax_tree {\n");

    _write_node(file, 0, tree);

    fprintf(file, "}\n");
}

void neart_write_syntax_tree_to_file(const char * filename, expr_t * tree) {
    FILE * file = fopen(filename, "w");

    _write_tree(file, tree);

    fflush(file);
    fclose(file);
}
