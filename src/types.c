#include "types.h"

#include <string.h>
#include "logging.h"

const char * builtin_types_name[] = {
    FOREACH_BUILTIN_TYPE(BUILTIN_SYNTAX_NAME)
};

const char builtin_types[] = {
    FOREACH_BUILTIN_TYPE(BUILTIN_TYPES_CHAR)
};

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
