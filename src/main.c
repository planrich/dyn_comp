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
    context_t ctx;

    ctx.root = expr_tree_alloc(TT_UNIT);
    yy = yyparse(&ctx);
    if (yy != 0)
    {
        printf("failed at line %d\n", yylineno);
        return 1;
    }
    else{
        printf("Success.\n");
    }
    expr_tree_free_r(ctx.root);

    return 0;
}
