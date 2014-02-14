#include "types.h"

#include <string.h>
#include "logging.h"
#include "gpir.h"

const char * builtin_types_name[] = {
    FOREACH_BUILTIN_TYPE(BUILTIN_SYNTAX_NAME)
};

const char builtin_types[] = {
    FOREACH_BUILTIN_TYPE(BUILTIN_TYPES_CHAR)
};

static char params_binary_int[] = { 0x2, 0x0, 0x0, 0x0, 
                                    0x8, 0x0, 
                                    0xc, 0x0,
                                    type_int, 0x0,
                                    ',', 0x0,
                                    type_int, 0x0,
                                    ',', 0x0,
                                    type_int, 0x0,
                                    ',', 0x0,
                                  };


#define BUILTIN_FUNCS(p1, _name, param_count, _params, idx, ...) \
    { .name = _name, .params = (params_t*)&_params, .symbols = NULL, .patterns = NULL },
func_t builtin_functions[BUILTIN_TYPE_COUNT] = {
    FOREACH_BUILTIN_FUNC(BUILTIN_FUNCS)
};

#define PULL_LOOKUP(p1, p2, p3, p4, idx, etype, ...) \
    case etype: return &builtin_functions[idx];

func_t * neart_builtin_func_lookup(expr_type_t type) {
    switch (type) {
        FOREACH_BUILTIN_FUNC(PULL_LOOKUP)
        default: return NULL;
    }
}

int neart_is_builtin_type(const char * name, type_t * type) {

    int i;

    if (strlen(name) < BUILTIN_NAME_MIN_LEN) {
        return 0;
    }

    const char * wptr = builtin_types_name[0];
    const char * ptr = name;

    for ( i = 0; i < BUILTIN_TYPE_COUNT ; ) {

        if (*wptr == *ptr) {
            if (*ptr == '\0') { *type = builtin_types[i]; return 1; }
            ptr++;
            wptr++;
        } else {
            i++;
            // even when they point to memory not in the
            // array it does not matter -> loop will terminate immediatly
            wptr = builtin_types_name[i];
            ptr = name;
        }
    }

    return 0;
}

