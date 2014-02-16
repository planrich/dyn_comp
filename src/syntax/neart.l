%{
#include "ast.h"
#include "neart.tab.h"


char * strdup(const char * s);
int fileno(FILE * name);

/* handle locations */
int yycolumn = 1;

#define YY_USER_ACTION yylloc->first_line = yylloc->last_line = yylineno; \
    yylloc->first_column = yycolumn; yylloc->last_column = yycolumn+yyleng-1; \
    yycolumn += yyleng;
%}

ID  [a-zA-Z_][a-zA-Z0-9_]*
DIGIT [0-9]

WS  [ \t]+
NL  (\n|\r\n)
STRING \"[^\"]\";

%option bison-bridge
%option bison-locations
%option ansi-definitions
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

{STRING} {
    yylval->text = strdup(yytext);
    return T_STR;
}

"[" { return T_OBRACKET; }
"]" { return T_CBRACKET; }
"{" { return T_OBRACE; }
"}" { return T_CBRACE; }
"(" { return T_OPARENS; }
")" { return T_CPARENS; }

"," { return T_COMMA; }
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
{NL} { yycolumn = 1; }

. { printf("unexpected input %s\n", yytext); exit(-1); }
