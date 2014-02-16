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

#define BUILTIN_SHORT_TYPE_CASE_INDEX(p1, p2, p3, idx) if (p2 == type) { return p3; }

const char * neart_builtin_type_name(type_t type) {
    FOREACH_BUILTIN_TYPE(BUILTIN_SHORT_TYPE_CASE_INDEX)
    return NULL;
}

const char * neart_type_name(type_t type) {
    const char * name = neart_builtin_type_name(type);
    // TODO handle builtin types

    return name;
}

static char params_binary_int[] = { 0x3, 0x0, 0x0, 0x0, 
                                    0xa, 0x0, 
                                    0xe, 0x0,
                                    0x12, 0x0,
                                    type_int, 0x0,
                                    ',', 0x0,
                                    type_int, 0x0,
                                    ',', 0x0,
                                    type_int, 0x0,
                                    ',', 0x0,
                                  };

#define BUILTIN_FUNCS(p1, p2, param_count, _params, idx, ...) \
    static func_t p1 = { .name = p2, .params = (params_t*)&_params, .symbols = NULL, .patterns = NULL };

FOREACH_BUILTIN_FUNC(BUILTIN_FUNCS);

#define PULL_LOOKUP(p1, p2, p3, p4, idx, etype, ...) \
    case etype: return &p1;

func_t * neart_builtin_func_lookup(expr_type_t type) {

    switch (type) {
        FOREACH_BUILTIN_FUNC(PULL_LOOKUP)
    }
    return NULL;
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

