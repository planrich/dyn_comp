%{

#include "neart.tab.h"
#include "lexer.h"

%}

ID  [a-zA-Z_][a-zA-Z0-9_]*
DIGIT [0-9]

WS  [ \t]+
NL  (\n|\r\n)

%%


"unit" { return T_UNIT; }
"version" { return T_VERSION; }
"func" { return T_FUNC; }

{ID} { 
    yylval.text = strdup(yytext);
    // TODO free?
    return T_ID;
}

{DIGIT}+ {
    //yylval.text = strdup(yytext);
    // TODO free?
    return T_INT;
}

{WS} {
    yylval.ws = strlen(yytext);
    return T_WS;
}

{NL} { return T_NL; }

"->" { return T_ARROW; }
":" { return T_COLON; }
"=" { return T_EQUAL; }
";" { return T_SEMI_COLON; }

. { printf("unexpected input %s\n", yytext); exit(-1); }
