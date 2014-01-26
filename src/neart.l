%{

#include "neart.tab.h"
#include "lexer.h"

%}

ID  [a-zA-Z_][a-zA-Z0-9_]*
DIGIT [0-9]

WS  [ \t]
NL  \n

%%

{WS}+ { 
    yylval.ws = strlen(yytext);
    return TOK_WS;
    //printf("ws %ld\n", strlen(yytext)); 
}

{NL} {
    return TOK_NL;
}

unit { return TOK_UNIT; }
version { return TOK_VERSION; }
func { return TOK_FUNC; }

{ID} { 
    yylval.text = strdup(yytext);
    // TODO free?
    return TOK_ID;
}

{DIGIT}+ {
    yylval.text = strdup(yytext);
    // TODO free?
    return TOK_INT;
    //printf("natural \n");
}

<<EOF>> { return TOK_EOF; }

":" { return TOK_COLON; }
"=" { return TOK_EQUAL; }
";" { return TOK_SEMI_COLON; }

. { printf("unexpected input\n"); exit(-1); }
