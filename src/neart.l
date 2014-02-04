%{

#include "structs.h"
#include "neart.tab.h"
#include "lexer.h"

%}

ID  [a-zA-Z_][a-zA-Z0-9_]*
DIGIT [0-9]

WS  [ \t]+
NL  (\n|\r\n)

%option bison-bridge
%option bison-locations
%option yylineno

%%


"unit" { return T_UNIT; }
"version" { return T_VERSION; }
"func" { return T_FUNC; }

{ID} { 
    yylval->text = strdup(yytext);
    return T_ID;
}

{DIGIT}+ {
    yylval->text = strdup(yytext);
    return T_INT;
}


"[" { return T_OBRACKET; }
"]" { return T_CBRACKET; }
"{" { return T_OBRACE; }
"}" { return T_CBRACE; }
"(" { return T_OPARENS; }
")" { return T_CPARENS; }

"_" { return T_UNDERSCORE; }
"-" { return T_MINUS; }
"+" { return T_PLUS; }
"*" { return T_STAR; }
"/" { return T_FSLASH; }
">" { return T_GT; }
":" { return T_COLON; }
"=" { return T_EQUAL; }
";" { return T_SEMI_COLON; }

{WS} { }
{NL} { }

. { printf("unexpected input %s\n", yytext); exit(-1); }
