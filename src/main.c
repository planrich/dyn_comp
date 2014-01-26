#include <stdio.h>
/* this file should work in both c and c++ */
extern int yyparse();
extern int yylineno;

int yyerror (char const * msg) {
    printf("failed at: (%s) %d\n", msg, yylineno);
    return 1;
}

int main(int argc, char *argv[])
{
    int yy;
    yy = yyparse();
    if (yy != 0)
    {
        printf("syntax error %d:%d. Aborting.\n", yylineno, 1);
        return 1;
    }
    else{
        printf("Success.\n");
    }
    return 0;
}
