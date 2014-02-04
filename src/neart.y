%{ 
#include <stdio.h>
#include "structs.h"
#include "neart.tab.h"
#include "logging.h"

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

%token T_PLUS
%token T_MINUS
%token T_STAR
%token T_FSLASH
%token T_GT

// TODO
%left T_FSLASH T_STAR T_MINUS T_PLUS
%right '('


%%

file:
    meta funclist {
      ctx->syntax_tree = $<expr>funclist;
    }
  ;

meta: 
    T_UNIT T_ID[module] {
        free($<text>module);
    }
  ;

funclist:
    func funclist {
        expr_t * func = $<expr>1;
        func->next = $<expr>2; 
        $<expr>$ = func;
    }
  | { $<expr>$ = NULL; }
  ;

func:
    T_FUNC T_ID[name] T_COLON params patterns {
        expr_t * func = neart_expr_alloc(ET_FUNC);
        expr_t * params = neart_expr_alloc(ET_PARAMS);

        func->data = $<text>name;
        func->detail = params;
        params->detail = $<expr>params;
        params->next = $<expr>patterns;

        $<expr>$ = func;
    }
  ;

patterns:
    T_EQUAL bindings T_SEMI_COLON expr patterns[patterns_p] {
        expr_t * pattern = neart_expr_alloc(ET_PATTERN);
        expr_t * bindings = neart_expr_alloc(ET_BINDINGS);
        pattern->next = $<expr>patterns_p;
        pattern->detail = bindings;
        bindings->detail = $<expr>bindings;
        bindings->next = $<expr>expr;
        $<expr>$ = pattern;
    }
  | { $<expr>$ = NULL; }
  ;

bindings:
    binding bindings { 
        expr_t * binding = neart_expr_alloc(ET_BINDING);
        binding->detail = $<expr>1;
        binding->next = $<expr>2;
        $<expr>$ = binding; 
    }
  | { $<expr>$ = NULL; }
  ;

var:
    T_ID[variable] {
        expr_t * binding = neart_expr_alloc(ET_VARIABLE);
        binding->data = $<text>variable;
        $<expr>$ = binding;
    }
  ;

binding:
    T_OPARENS binding T_CPARENS {
        $<expr>$ = $<expr>2;
    }
  | var { $<expr>$ = $<expr>var; } 
  | T_INT[num] {
        expr_t * binding = neart_expr_alloc(ET_INTEGER);
        binding->data = $<text>num;
        $<expr>$ = binding;
    }
  | var T_COLON binding[binding_p]  { 
        expr_t * binding = neart_expr_alloc(ET_CONS);
        binding->left = $<expr>var;
        binding->right = $<expr>binding_p;
        $<expr>$ = binding;
    }
  | T_UNDERSCORE { $<expr>$ = neart_expr_alloc(ET_MATCH_ANY); }
  | T_OBRACKET binding T_CBRACKET {
        expr_t * binding = neart_expr_alloc(ET_LIST);
        binding->detail = $<expr>2;
        $<expr>$ = binding;
    }
  | nil { $<expr>$ = neart_expr_alloc(ET_NIL); }
  ;

binary_op:
    T_PLUS   { $<expr>$ = neart_expr_alloc(ET_OP_IADD); }
  | T_MINUS  { $<expr>$ = neart_expr_alloc(ET_OP_ISUB); }
  | T_STAR   { $<expr>$ = neart_expr_alloc(ET_OP_IMUL); }
  | T_FSLASH { $<expr>$ = neart_expr_alloc(ET_OP_IDIV); }
  ;

op:
    expr binary_op expr {
        expr_t * expr = $<expr>binary_op;
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | T_MINUS expr {
        expr_t * expr = neart_expr_alloc(ET_NEGATIVE);
        expr->detail= $<expr>2;
        $<expr>$ = expr; 
    }
  ;

expr:
    T_ID { 
        expr_t * expr = neart_expr_alloc(ET_VARIABLE);
        expr->data = $<text>1;
        $<expr>$ = expr; 
    }
  | op { $<expr>$ = $<expr>op; }
  | T_OPARENS expr T_CPARENS {
        expr_t * expr = neart_expr_alloc(ET_PARENS);
        expr->detail = $<expr>2;
        $<expr>$ = expr; 
    }
  | T_INT { 
        expr_t * expr = neart_expr_alloc(ET_INTEGER);
        expr->data = $<text>1;
        $<expr>$ = expr; 
    }
  | nil { $<expr>$ = neart_expr_alloc(ET_NIL); }
  ;

params:
    param T_MINUS T_GT params[params_p] {
        expr_t * param = $<expr>param;
        param->next = $<expr>params_p;
        $<expr>$ = param;
    }
  | param  {
        $<expr>$ = $<expr>param;
    }
  ;

nil:
    T_OBRACKET T_CBRACKET
  ;

param:
    T_OBRACKET param T_CBRACKET {
        expr_t * param = neart_expr_alloc(ET_LIST);
        param->detail = $<expr>2;
        $<expr>$ = param;
    }
  | var { $<expr>$ = $<expr>var; }
  ;


%%

