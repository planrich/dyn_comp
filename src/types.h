#ifndef _TYPES_H_
#define _TYPES_H_

// do not add ',' as char! colon is a separator
#define FOREACH_BUILTIN_TYPE(PBT) \
    PBT(type_int8,    't',    "int8"), \
    PBT(type_int16,   's',    "int16"), \
    PBT(type_int32,   'i',    "int32"), \
    PBT(type_int,     'i',    "int"), \
    PBT(type_int64,   'l',    "int64"), \
    PBT(type_str,     's',    "str"), \
    PBT(type_bin,     'b',    "bin"), \
    PBT(type_list,    '[',    "list"), \
    PBT(type_func,    '(',    "func"), \
    PBT(type_generic, 'g',    "generic"), \
    PBT(type_data,    'd',    "data"), \

#define BUILTIN_ENUM(p1, p2, ...) p1 = p2

typedef enum builtin_types_t {
    FOREACH_BUILTIN_TYPE(BUILTIN_ENUM)
} builtin_types_t;

#endif /* _TYPES_H_ */
