
#include "gpir.h"

#include "utils.h"
#include "kstring.h"
#include "klist.h"
#include "sem.h"
#include "logging.h"
#include "ast.h"
#include <errno.h>

char * strdup(const char * s);

//////////////////////////////////////// module

module_t * neart_module_alloc(const char * name) {
    ALLOC_STRUCT(module_t, m);
    m->name = strdup(name);
    m->symbols = NULL;
    return m;
}

void neart_module_free(module_t * mod) {
    if (mod->symbols != NULL) {
        neart_sym_table_free(mod->symbols);
    }
    free((void*)mod->name);
    free(mod);
}

//////////////////////////////////////// pattern

pattern_t * neart_pattern_alloc(sem_post_expr_t * expr) {
    ALLOC_STRUCT(pattern_t, pat);
    pat->expr = expr;
    pat->bindings = NULL;
    pat->symbols = NULL;
    return pat;
}

void neart_pattern_free(pattern_t * pattern) {
    {
        sem_post_expr_t * it = pattern->expr;
        sem_post_expr_t * tmp;

        while (it != NULL) {

            neart_expr_free(it->expr);

            tmp = it;
            it = it->next;

            free(tmp);
        }
    }
    free(pattern);
}

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

    // add it to the current symbol table
    sym_entry_t entry;
    entry.func = func;
    entry.entry_type = SYM_FUNC;
    entry.type = 0;
    if (neart_sym_table_insert(cc->symbols, func->name, entry)) {
        errno = ERR_FUNC_ALREADY_DEF;
        NEART_LOG_FATAL("function %s is already defined in module %s\n", func->name, mod->name);
    }
}

//////////////////////////////////////// param

/**
 * each node takes on slot, for each func (parens) an
 * additional slot is reserved for termination
 */
static int _expr_node_count(expr_t * e) {
    if (e == NULL) { return 0; }
    int i = 1;
    if (e->type == ET_PARENS) {
        i++;
    }
    return i + _expr_node_count(e->left) + _expr_node_count(e->right);
}

static int _generic_param_defined(expr_t * expr, int first_n_params) {
    return -1;
}

#define __char_free(x)
KLIST_INIT(string, char*, __char_free);

// temp structure holding the context of construction
typedef struct __param_transform {
    expr_t * first_param;
    expr_t * cur;
    params_t * params;
    param_t * param;
    param_offset_t * offset;
    int param_index;
    klist_t(string) * generic_params;
    uint8_t sub_param_count;
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

    if (*(param-NEART_PARAM_SIZE) == type_func) {
        // the param count is the param count minus the ones in between!
        uint8_t count = (uint8_t)(c->param - param)/NEART_PARAM_SIZE;
        *(param-1) = count - c->sub_param_count;
        c->sub_param_count = count;
    }
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
        c->sub_param_count = 0;
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
                   + sizeof(param_t) * (nodes + colons) * 2 /* bytes describing the types. 1 byte char, 1 byte id */
                   );

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

    return params;
}

params_t * neart_params_anon_func(param_t * old) {
    params_t * params;
    int nodes = 0;

    // jump over the first parens (
    param_t * it = neart_param_next(old);
    int param_count = 0;

    while (!neart_param_end(it)) {
        param_count++;
        nodes++;
        while (neart_param_type(it) == type_func) {
            nodes += neart_param_idx(it);
            it += neart_param_idx(it) * NEART_PARAM_SIZE;
        }
        if (it != NULL) {
            it = neart_param_next(it);
        }
    }

    // param count is at least 1
    int colons = param_count;
    params = malloc( sizeof(params_t) /* count */ 
                   + sizeof(param_offset_t) * param_count /* offsets to parameters */
                   + sizeof(param_t) * (nodes + colons) * 2 /* bytes describing the types. 1 byte char, 2 byte id */
                   );


    *params = param_count;

    param_offset_t * off = (param_offset_t*) (params+1);

    param_t * copy = (param_t*) off + param_count * sizeof(param_offset_t);

    it = old;

    // copy the entry
    while (!neart_param_end(it)) {
        // write the offset and copy until a ','
        *off++ = (param_offset_t)(((void*)copy) - ((void*)params));
        *copy++ = *(it);
        *copy++ = *(it+1);

        // when encountering a function
        // copy until all params are copied
        // (2 g0 (2 g1 g2 will write 6 symbols
        if (neart_param_type(it) == type_func) {
            int to_cpy = neart_param_idx(it);
            it = neart_param_next(it);
            while (to_cpy > 0) {
                if (neart_param_type(it) == type_func) {
                    to_cpy += neart_param_idx(it);
                }

                *copy++ = *it++;
                *copy++ = *it++;

                to_cpy--;
            }
        } else {
            it = neart_param_next(it);
        }
        // terminate parameter
        *copy++ = ',';
        *copy++ = 0;
    }

    return params;
}

param_t * neart_param_at(params_t * params, int idx, int nesting) {

    if (idx < 0 && *params <= idx) {
        return NULL;
    }

    param_offset_t * off = (param_offset_t*)(params+1); // note: cast binds stronger than +1
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


void neart_params_debug_print(params_t * params) {

    int i;
    int j;

    NEART_LOG_DEBUG("params: %d|", *params);
    param_offset_t * ptr = (param_offset_t*) (params+1);
    for (i = 0; i < *params; i++ ) {
        NEART_LOG_DEBUG("%d|", *ptr);
        ptr++;
    }
    for (i = 0; i < *params; i++) {
        param_t * par = neart_param_at(params, i, 0);
        while (!neart_param_end(par)) { 
            NEART_LOG_DEBUG(" %c%d", *par, *(par+1));
            par = neart_param_next(par);
        }
        NEART_LOG_DEBUG("%c", *par);
    }
    NEART_LOG_DEBUG("\n");

}
