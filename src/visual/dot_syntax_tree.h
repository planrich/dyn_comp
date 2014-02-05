
#ifndef _DOT_SYNTAX_TREE_H_
#define _DOT_SYNTAX_TREE_H_

#include "ast.h"

void neart_write_syntax_tree_to_file(const char * filename, expr_t * tree, const char * filter_func);

#endif
