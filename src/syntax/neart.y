%{ 
#include <stdio.h>
#include "ast.h"
#include "neart.tab.h"
#include "logging.h"

int yylex ( YYSTYPE * lvalp, YYLTYPE * llocp );

int yyerror (YYLTYPE *locp, expr_t * root, char const * msg);

#define CPY_LOC(obj, l1, l2) \
    obj->first_line = l1.first_line; \
    obj->last_line = l2.last_line; \
    obj->first_column = l1.first_column; \
    obj->last_column = l2.last_column;

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
%token T_IF
%token T_ELSE
%token T_THEN

// numeric
%token<text> T_INT

%token<text> T_ID

%token<text> T_STR;

// punctuation
%token T_COLON
%token T_EQUAL
%token T_DOUBLE_EQUAL
%token T_SEMI_COLON
%token T_COMMA
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

%left DELSE
%left T_DOUBLE_EQUAL
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
        CPY_LOC(func, @$, @$);
        CPY_LOC(params, @4, @4);

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
        CPY_LOC(pattern, @1, @4);
        expr_t * bindings = neart_expr_alloc(ET_BINDINGS);
        CPY_LOC(bindings, @2, @2);
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
        CPY_LOC(binding, @1, @1);
        binding->detail = $<expr>1;
        binding->next = $<expr>2;
        $<expr>$ = binding; 
    }
  | { $<expr>$ = NULL; }
  ;

var:
    T_ID[variable] {
        expr_t * var = neart_expr_alloc(ET_VARIABLE);
        CPY_LOC(var, @1, @1);
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
        CPY_LOC(binding, @1, @1);
        binding->data = $<text>num;
        $<expr>$ = binding;
    }
  | binding[left] T_COLON binding[right]  { 
        expr_t * binding = neart_expr_alloc(ET_CONS);
        CPY_LOC(binding, @1, @1);
        binding->left = $<expr>left;
        binding->right = $<expr>right;
        $<expr>$ = binding;
    }
  | T_UNDERSCORE { 
        expr_t * expr = neart_expr_alloc(ET_MATCH_ANY); 
        CPY_LOC(expr, @1, @1);
        $<expr>$ = expr;
    }
  | T_OBRACKET binding T_CBRACKET {
        expr_t * binding = neart_expr_alloc(ET_LIST);
        CPY_LOC(binding, @2, @2);
        binding->detail = $<expr>2;
        $<expr>$ = binding;
    }
  | nil { 
        expr_t * expr = neart_expr_alloc(ET_NIL); 
        CPY_LOC(expr, @1, @1);
        $<expr>$ = expr;
    }
  ;

expr:
    T_OPARENS expr T_CPARENS flist {
        expr_t * parens = neart_expr_alloc(ET_PARENS);
        CPY_LOC(parens, @1, @3);
        expr_t * expr = $<expr>2;
        parens->detail = expr;
        parens->next = $<expr>4;
        $<expr>$ = parens;
    }
  | expr T_PLUS expr {
        expr_t * expr = neart_expr_alloc(ET_OP_IADD);
        CPY_LOC(expr, @1, @3);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | expr T_STAR expr {
        expr_t * expr = neart_expr_alloc(ET_OP_IMUL);
        CPY_LOC(expr, @1, @3);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | expr T_FSLASH expr {
        expr_t * expr = neart_expr_alloc(ET_OP_IDIV);
        CPY_LOC(expr, @1, @3);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | expr T_MINUS expr {
        expr_t * expr = neart_expr_alloc(ET_OP_ISUB);
        CPY_LOC(expr, @1, @3);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | expr T_COLON expr {
        expr_t * expr = neart_expr_alloc(ET_OP_CONS);
        CPY_LOC(expr, @1, @3);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | expr T_DOUBLE_EQUAL expr {
        expr_t * expr = neart_expr_alloc(ET_OP_EQUAL);
        CPY_LOC(expr, @1, @3);
        expr->left = $<expr>1;
        expr->right = $<expr>3;
        $<expr>$ = expr; 
    }
  | T_IF expr T_THEN expr T_ELSE expr %prec DELSE {
        expr_t * _if = neart_expr_alloc(ET_IF);
        CPY_LOC(_if, @1, @6);
        _if->left = $<expr>2;

        expr_t * then = neart_expr_alloc(ET_THEN);
        CPY_LOC(then, @4, @4);
        then->left = $<expr>4;

        expr_t * _else = neart_expr_alloc(ET_ELSE);
        CPY_LOC(then, @6, @6);
        _else->left = $<expr>6;

        _if->right = then;
        then->right = _else;

        $<expr>$ = _if; 
    }
  | T_MINUS expr %prec UNARY_MINUS {
        expr_t * expr = neart_expr_alloc(ET_NEGATIVE);
        CPY_LOC(expr, @1, @2);
        expr->detail= $<expr>2;
        $<expr>$ = expr; 
    }
  | T_ID flist  { 
        expr_t * expr = neart_expr_alloc(ET_VARIABLE);
        CPY_LOC(expr, @1, @1);
        expr->data = $<text>1;
        expr->next = $<expr>2;
        $<expr>$ = expr; 
    }
  | nil { 
        expr_t * expr = neart_expr_alloc(ET_NIL); 
        CPY_LOC(expr, @1, @1);
        $<expr>$ = expr;
    }
  | T_INT flist { 
        expr_t * expr = neart_expr_alloc(ET_INTEGER);
        CPY_LOC(expr, @1, @1);
        expr->data = $<text>1;
        expr->next = $<expr>2;
        $<expr>$ = expr; 
    }
  | T_STR { 
        expr_t * expr = neart_expr_alloc(ET_STRING);
        CPY_LOC(expr, @1, @1);
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
        CPY_LOC(param, @1, @3);
        param->detail = $<expr>2;
        $<expr>$ = param;
    }
  | T_OPARENS params T_CPARENS {
        expr_t * param = neart_expr_alloc(ET_PARENS);
        CPY_LOC(param, @1, @3);
        param->detail = $<expr>params;
        $<expr>$ = param;
    }
  | var { $<expr>$ = $<expr>var; }
  ;


%%

