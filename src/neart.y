%{ 
#include <stdio.h>
#include "ast.h"
#include "neart.tab.h"
#include "logging.h"

int yylex ( YYSTYPE * lvalp, YYLTYPE * llocp );

int yyerror (YYLTYPE *locp, expr_t * root, char const * msg);

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

%parse-param { expr_t * root }

// keywords
%token T_UNIT
%token T_VERSION
%token T_FUNC

// numeric
%token<text> T_INT

%token<text> T_ID

%token<text> T_STR;

// punctuation
%token T_COLON
%token T_EQUAL
%token T_SEMI_COLON
%token T_COMMA;
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

%left T_COLON
%left T_MINUS 
%left T_PLUS 
%left T_FSLASH 
%left T_STAR
%left UNARY_MINUS

%precedence "flist"
%precedence "expr"

%%

file:
    meta funclist {
      root->detail = $<expr>funclist;
      root->data = $<text>meta;
    }
  ;

meta: 
    T_UNIT T_ID[module] {
        $<text>$ = $<text>module;
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
        expr_t * var = neart_expr_alloc(ET_VARIABLE);
        var->data = $<text>variable;
        $<expr>$ = var;
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

expr:
    T_OPARENS expr T_CPARENS {
        $<expr>$ = $<expr>2; 
    }
  | expr T_PLUS expr {
        expr_t * expr = neart_expr_alloc(ET_OP_IADD);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | expr T_STAR expr {
        expr_t * expr = neart_expr_alloc(ET_OP_IMUL);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | expr T_FSLASH expr {
        expr_t * expr = neart_expr_alloc(ET_OP_IDIV);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | expr T_MINUS expr {
        expr_t * expr = neart_expr_alloc(ET_OP_ISUB);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | expr T_COLON expr {
        expr_t * expr = neart_expr_alloc(ET_OP_CONS);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | T_MINUS expr %prec UNARY_MINUS {
        expr_t * expr = neart_expr_alloc(ET_NEGATIVE);
        expr->detail= $<expr>2;
        $<expr>$ = expr; 
    }
  | T_ID flist  { 
        expr_t * expr = neart_expr_alloc(ET_VARIABLE);
        expr->data = $<text>1;
        expr->next = $<expr>2;
        $<expr>$ = expr; 
    }
  | nil { $<expr>$ = neart_expr_alloc(ET_NIL); }
  | T_INT { 
        expr_t * expr = neart_expr_alloc(ET_INTEGER);
        expr->data = $<text>1;
        //expr->next = $<expr>2;
        $<expr>$ = expr; 
    }
  | T_STR { 
        expr_t * expr = neart_expr_alloc(ET_STRING);
        expr->data = $<text>1;
        $<expr>$ = expr; 
    }
  ;

flist: 
    expr %prec "expr" { 
        expr_t * expr = $<expr>expr; 
        //expr->next=$<expr>2; 
        $<expr>$ = expr; 
    } 
  | %prec "expr" { $<expr>$ = NULL; } 
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
    T_OBRACKET T_CBRACKET { $<expr>$ = NULL; }
  ;

param:
    T_OBRACKET param T_CBRACKET {
        expr_t * param = neart_expr_alloc(ET_LIST);
        param->detail = $<expr>2;
        $<expr>$ = param;
    }
  | T_OPARENS params T_CPARENS {
        expr_t * param = neart_expr_alloc(ET_PARENS);
        param->detail = $<expr>params;
        $<expr>$ = param;
    }
  | var { $<expr>$ = $<expr>var; }
  ;


%%

