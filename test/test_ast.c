
#include <stdio.h>
#include "logging.h"
#include "config.h"
#include "test_config.h"
#include "structs.h"

extern int yyparse();
extern int yylineno;

int yyerror (YYLTYPE *locp, context_t * ctx, char const * msg) {
    printf("!> %d:%d %s\n", yylineno, locp->first_column, msg);
    return 1;
}

int main() {

    return 0;
}
