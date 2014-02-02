%{ 
#include <stdio.h>
#include "structs.h"
#include "neart.tab.h"

int yylex ( YYSTYPE * lvalp, YYLTYPE * llocp );

int yyerror (YYLTYPE *locp, context_t * ctx, char const * msg);

%}

%locations 
%define api.pure full
%define parse.error verbose

%union {
    char * text;
    expr_t * expr;
    void * generic;
}

%start file

%verbose

%parse-param { context_t * ctx }

// keywords
%token T_UNIT
%token T_VERSION
%token T_FUNC

// numeric
%token T_INT

%token<text> T_ID

// punctuation
%token T_COLON
%token T_EQUAL
%token T_SEMI_COLON

%token T_UNDERSCORE

%token T_OBRACKET
%token T_CBRACKET
%token T_OBRACE
%token T_CBRACE
%token T_OPARENS
%token T_CPARENS

%token T_MINUS
%token T_PLUS
%token T_GT

%left '+' '-'
%left '*' '/'
%right '('


%%

file:
    meta funclist
  ;

meta: 
    T_UNIT T_ID[module]
  ;

funclist:
    func funclist
  |
  ;

func:
    T_FUNC T_ID[name] { 
        func_t * func = func_alloc($name);
        context_add_function(ctx, func);
    } T_COLON params patterns
  ;

patterns:
    T_EQUAL {
      $<generic>pat = pattern_alloc();
      ctx->last_pattern = (pattern_t*)$<generic>pat;
    }[pat] bindings T_SEMI_COLON expr {
      ((pattern_t*)$<generic>pat)->expr = $<expr>expr;
    } patterns
  |
  ;

bindings:
    binding { pattern_add_binding(ctx->last_pattern, $<expr>binding);  } bindings
  |
  ;

binding:
    T_OPARENS binding T_CPARENS
  | T_ID
  | T_ID T_COLON binding 
  | T_UNDERSCORE
  | T_OBRACKET binding T_CBRACKET
  | nil
  ;


expr:
    T_ID
  | T_INT
  | nil
  ;

params:
    param T_MINUS T_GT params
  | param 
  ;

nil:
    T_OBRACKET T_CBRACKET
  ;

param:
    T_OBRACKET param T_CBRACKET
  | T_ID
  ;


%%

