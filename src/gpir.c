
#include "gpir.h"

#include <errno.h>
#include "utils.h"
#include "kstring.h"
#include "klist.h"
#include "sem.h"
#include "logging.h"
#include "ast.h"


//////////////////////////////////////// module

module_t * neart_module_alloc(const char * name) {
    ALLOC_STRUCT(module_t, m);
    m->func_table = kh_init(str_func_t);
    m->symbols = neart_sym_table_alloc();
    m->name = strdup(name);
    return m;
}

void neart_module_free(module_t * mod) {
    NEART_LOG_TRACE();
    khiter_t k;
    khash_t(str_func_t) * h = mod->func_table;

    for (k = kh_begin(h) ; k < kh_end(h); ++k) {
        if (kh_exist(h, k)) {
            func_t * f = kh_value(h, k);
            neart_func_free(f);
            kh_value(h, k) = NULL;
        }
    }
    kh_destroy(str_func_t, h);
    neart_sym_table_free(mod->symbols);
    free((void*)mod->name);
    free(mod);
}

//////////////////////////////////////// pattern

pattern_t * neart_pattern_alloc(klist_t(expr_t) * postfix) {
    ALLOC_STRUCT(pattern_t, pat);
    pat->expr_postfix = postfix;
    pat->bindings = NULL;
    return pat;
}

void neart_pattern_free(pattern_t * pattern) {
    {
        kliter_t(expr_t) * it;
        klist_t(expr_t) * l = pattern->expr_postfix;

        for (it = kl_begin(l); it != kl_end(l); it = kl_next(it)) {
            expr_t * expr = kl_val(it);
            neart_expr_free(expr);
        }
        kl_destroy(expr_t, pattern->expr_postfix);
    }
    free(pattern);
}
/*

void pattern_add_binding(pattern_t * pattern, expr_t * expr) {
    *kl_pushp(expr_t, pattern->bindings) = expr;
}*/

//////////////////////////////////////// func

func_t * neart_func_alloc(const char * name) {

    ALLOC_STRUCT(func_t, f);

    f->name = strdup(name);
    f->params = NULL;
    f->patterns = kl_init(pattern_t);
    f->symbols = NULL;
    return f;
}

void neart_func_free(func_t * func) {

    free((void*)func->name);

    {
        kliter_t(pattern_t) * it;
        klist_t(pattern_t) * l = func->patterns;
        for (it = kl_begin(l); it != kl_end(l); it = kl_next(it)) {
            pattern_t * pattern = kl_val(it);
            neart_pattern_free(pattern);
        }

        kl_destroy(pattern_t,func->patterns);
    }

    if (func->params != NULL) {
        neart_params_free(func->params);
    }

    if (func->symbols != NULL) {
        neart_sym_table_free(func->symbols);
    }

    free(func);
}

inline
void neart_func_add_pattern(func_t * func, pattern_t * pattern) {
    *kl_pushp(pattern_t, func->patterns) = pattern;
}


void neart_module_add_function(compile_context_t * cc, module_t * mod, func_t * func) {
    int ret;
    khint_t k;

    k = kh_put(str_func_t, mod->func_table, func->name, &ret);
    if (!ret) {
        errno = ERR_FUNC_ALREADY_DEF;
        NEART_LOG(LOG_FATAL, "function %s is already defined in module %s\n", func->name, mod->name);
    } else {
        kh_value(mod->func_table, k) = func;
    }

    // add it to the current symbol table
    sym_entry_t entry;
    entry.func = func;
    entry.entry_type = SYM_FUNC;
    entry.type = 0;
    neart_sym_table_insert(mod->symbols, func->name, entry);
}

//////////////////////////////////////// param

static int _expr_node_count(expr_t * e) {
    if (e == NULL) { return 0; }
    return 1 + _expr_node_count(e->left) + _expr_node_count(e->right);
}

static int _generic_param_defined(expr_t * expr, int first_n_params) {

    return -1;
}

#define __char_free(x)
KLIST_INIT(string, char*, __char_free);

typedef struct __param_transform {
    expr_t * first_param;
    expr_t * cur;
    params_t * params;
    param_t * param;
    param_offset_t * offset;
    int param_index;
    klist_t(string) * generic_params;
} _ptrans_t;

static int _param_find_already_declared(_ptrans_t * c) {

    klist_t(string) * l = c->generic_params;
    kliter_t(string) * it = kl_begin(l);
    int i = 0;

    while (it != kl_end(l)) {
        const char * name = kl_val(it);
        if (strcmp(name, c->cur->data) == 0) {
            return i;
        }

        i++;
        it = kl_next(it);
    }

    *kl_pushp(string, l) = c->cur->data;

    return i;
}
static void _params_transform_param(_ptrans_t * c) {

    param_t * param = c->param;
    expr_t * expr = c->cur;
    if (expr == NULL) { return; }

    switch (expr->type) {
        case ET_VARIABLE: { 
            // check if it is builtin type
            type_t type;

            if (neart_is_builtin_type(expr->data, &type)) {
                *param++ = type;
                *param++ = 0;
            } else {
                int idx = _param_find_already_declared(c);
                *param++ = type_generic;
                *param++ = idx;
            }
            break; 
        }
        case ET_LIST:
            *param++ = type_list;
            *param++ = 0;
            break;
        case ET_PARENS: 
            *param++ = type_func;
            *param++ = 0;
            break;
        default: { 
           NEART_LOG_FATAL("did not implement type %s\n", expr_type_names[expr->type]);
           exit(1);
        }
    };

    c->param = param;
    c->cur = expr->left;
    _params_transform_param(c);
    c->cur = expr->right;
    _params_transform_param(c);
}

static void _params_transform(_ptrans_t * c) {

    expr_t * next = c->cur;
    expr_t * tmp = NULL;

    while (next != NULL) {
        *c->offset++ = (param_offset_t)(((void*)c->param) - ((void*)c->params));

        tmp = next->next;
        // unhinge -> must not follow next chain in transform
        next->next = NULL; 

        c->cur = next;
        _params_transform_param(c);

        *c->param++ = ',';
        *c->param++ = 0;
        c->param_index++;

        next->next = tmp; // hinge again
        next = tmp;
    }
}

params_t * neart_params_transform(module_t * module, expr_t * param_expr, int * param_count) {

    params_t * params;
    int nodes = 0;

    ITER_EXPR_NEXT(param_expr, p, it)
        (*param_count)++;
        nodes += _expr_node_count(p);
    ITER_EXPR_END(it)

    // param count is at least 1
    int colons = *param_count;
    params = malloc( sizeof(params_t) /* count */ 
                   + sizeof(param_offset_t) * *param_count /* offsets to parameters */
                   + sizeof(param_t) * (nodes + colons) * 2 ) /* bytes describing the types. 1 byte char, 2 byte id */;

    *params = *param_count;

    param_offset_t * off = (param_offset_t*) (params+1);

    param_t * param = (param_t*) off + (*param_count) * sizeof(param_offset_t);

    _ptrans_t trans;
    trans.first_param = param_expr;
    trans.cur = param_expr;
    trans.params = params;
    trans.param = param;
    trans.offset = off;
    trans.param_index = 0;
    trans.generic_params = kl_init(string);
    _params_transform(&trans);

    kl_destroy(string, trans.generic_params);


    {
        int i;

        NEART_LOG_DEBUG("parameter trans to: %d|", *params);
        param_offset_t * ptr = (param_offset_t*) (params+1);
        for (i = 0; i < *params; i++ ) {
            NEART_LOG_DEBUG("%d|", *ptr);
            ptr++;
        }
        param_t * par = (param_t*)ptr;
        for (i = 0; i < *params;) {
            NEART_LOG_DEBUG("%c%d ", *par, *(par+1));
            if (*par == ',') {
                i++;
            }
            par+=2;
        }
        NEART_LOG_DEBUG("\n");
    }

    return params;
}



param_t * neart_param_at(params_t * params, int idx, int nesting) {

    if (idx < 0 && *params <= idx) {
        return NULL;
    }

    param_offset_t * off = (param_offset_t*)(params+1); // note: cast binds stronge than +1
    off += idx;

    param_t * param = ((param_t*)params) + *off;

    while (!neart_param_end(param)) {
        if (nesting == 0) {
            return param;
        }
        nesting--;
        param = neart_param_next(param);
    }

    return NULL;
}
