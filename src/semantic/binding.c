
#include "binding.h"

#include "logging.h"

const char * BINDING_WILDCARD_STR = "_";

static int _declare_bindings(compile_context_t * cc, params_t * params, expr_t * binding, int param_index, int depth) {

    if (binding == NULL) {
        return 0;
    }

    const char * name = binding->data;
    if (binding->type == ET_VARIABLE) {
        if (strcmp(name, BINDING_WILDCARD_STR) == 0) {
            return 0;
        }

        param_t * param = neart_param_at(params, param_index, depth);
        if (param == NULL) {
            NEART_LOG_FATAL("parameter %d at nesting %d not available\n", param_index, depth);
            return ERR_TYPE_TO_FEW_PARAMETER;
        }

        type_t type = neart_param_type(param);
        NEART_LOG_DEBUG("binding defining %s as %c\n", name, type);

        // consider this? can one unpack a function (a -> (b -> c)) unpack (a:bc)?
        param_t * top_param = neart_param_at(params, param_index, 0);
        if (neart_param_type(top_param) == type_func && depth > 0) {
            NEART_LOG_FATAL("tried to unpack func as you do with lists!\n");
            return ERR_TYPE_MATCH_LIST_WITH_NON_LIST;
        }

        sym_table_t * table = cc->symbols;
        sym_entry_t entry;
        entry.type = type;

        // anon functions bind
        if (entry.type == type_func) {
            entry.entry_type = SYM_ANON_FUNC;
        } else {
            entry.entry_type = SYM_VAR;
            entry.var = binding;
        }
        entry.param = param;

        neart_sym_table_insert(table, name, entry);
    }

    int err = _declare_bindings(cc, params, binding->left, param_index, depth + ((binding->type == ET_CONS) ? 1 : 0));
    if (err) {
        return err;
    }
    return _declare_bindings(cc, params, binding->right, param_index, depth);
}

semantic_error_t neart_declare_all_bindings(compile_context_t * cc, params_t * params, 
        expr_t * binding, int * binding_count) {
    int param_index = 0;
    expr_t * next = binding;
    expr_t * tmp;
    semantic_error_t err;

    while (next != NULL) {
        tmp = next->next;
        next->next = NULL; // unhinge to not recurse in _declare_bindings

        err = _declare_bindings(cc, params, next, param_index++, 0);
        if (err) { return err; }

        next->next = tmp;
        next = tmp;
    }
    *binding_count = param_index;

    return NO_ERROR;
}
