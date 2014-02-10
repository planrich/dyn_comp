#ifndef _TYPES_H_
#define _TYPES_H_

typedef char type_t;

#define BUILTIN_TYPE_COUNT (11)
#define BUILTIN_NAME_MIN_LEN (3)
// do not add ',' as char! colon is a separator
// NOTE: order by 3 parameter -> need to find type efficient
#define FOREACH_BUILTIN_TYPE(PBT) \
    PBT(type_bin,     'b',    "bin"), \
    PBT(type_data,    'd',    "data"), \
    PBT(type_func,    '(',    "func"), \
    PBT(type_generic, 'g',    "generic"), \
    PBT(type_int,     'i',    "int"), \
    PBT(type_int16,   's',    "int16"), \
    PBT(type_int32,   'i',    "int32"), \
    PBT(type_int8,    't',    "int8"), \
    PBT(type_int64,   'l',    "int64"), \
    PBT(type_list,    '[',    "list"), \
    PBT(type_str,     's',    "str"), \

#define BUILTIN_ENUM(p1, p2, ...) p1 = p2
#define BUILTIN_TYPES_CHAR(p1, p2, ...) p2
#define BUILTIN_SYNTAX_NAME(p1, p2, p3, ...) p3

typedef enum builtin_types_t {
    FOREACH_BUILTIN_TYPE(BUILTIN_ENUM)
} builtin_types_t;

const char * builtin_types_name[BUILTIN_TYPE_COUNT];
const char builtin_types[BUILTIN_TYPE_COUNT];

int neart_is_builtin_type(const char * name, type_t * type);

#endif /* _TYPES_H_ */
