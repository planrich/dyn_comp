#include <stdio.h>
#include "structs.h"
#include "neart.tab.h"

extern int yyparse();
extern int yylineno;

int yyerror (YYLTYPE *locp, context_t * ctx, char const * msg) {
    printf("!> %d:%d %s\n", yylineno, locp->first_column, msg);
    return 1;
}

int main(int argc, char *argv[])
{
    int yy;
    context_t * ctx = neart_context_alloc("stdin");

    yy = yyparse(ctx);
    if (yy != 0)
    {
        printf("failed at line %d\n", yylineno);
        return 1;
    }
    else{
        printf("Success.\n");
        neart_write_syntax_tree_to_file("syntax_tree.dot", ctx->syntax_tree);
    }

    neart_context_free(ctx);

    return 0;
}
