%{

#include "neart.tab.h"
#include "lexer.h"

%}

ID  [a-zA-Z_][a-zA-Z0-9_]*
DIGIT [0-9]

WS  [ \t]
NL  \n

SPECIAL [:;.,=(){}]

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

{SPECIAL} {
    //printf("special \n");
}

: { return TOK_DEF; }
= { return TOK_EQ; }
; { return TOK_SC; }

. { printf("unexpected input\n"); exit(-1); }
