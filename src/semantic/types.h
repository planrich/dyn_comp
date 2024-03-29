#ifndef _TYPES_H_
#define _TYPES_H_

typedef char type_t;

#define BUILTIN_TYPE_COUNT (12)
#define BUILTIN_NAME_MIN_LEN (3)

// do not add ',' and '$' as char! colon is a separator
// NOTE: order by 3 parameter -> need to find type efficient
#define FOREACH_BUILTIN_TYPE(PBT) \
    PBT(type_bin,     'B',    "bin",        0) \
    PBT(type_bool,    'b',    "bool",       1) \
    PBT(type_data,    'd',    "data",       2) \
    PBT(type_func,    '(',    "func",       3) \
    PBT(type_generic, 'g',    "generic",    4) \
    PBT(type_int,     'i',    "int",        5) \
    PBT(type_int16,   '2',    "int16",      6) \
    PBT(type_int32,   'i',    "int32",      7) \
    PBT(type_int8,    't',    "int8",       8) \
    PBT(type_int64,   'l',    "int64",      9) \
    PBT(type_list,    '[',    "list",       10) \
    PBT(type_str,     's',    "str",        11) \

#define BUILTIN_ENUM(p1, p2, ...) p1 = p2,


#define type_func_builtin ('$')
#define type_none ('\0')

typedef enum builtin_types_t {
    FOREACH_BUILTIN_TYPE(BUILTIN_ENUM)
} builtin_types_t;

const char * builtin_types_name[BUILTIN_TYPE_COUNT];
const char builtin_types[BUILTIN_TYPE_COUNT];

const char * neart_type_name(type_t type);

int neart_is_builtin_type(const char * name, type_t * type);

//////////////////////////////////////// funcs

#define BUILTIN_FUNC_COUNT (5)

#define FOREACH_BUILTIN_FUNC(P) \
    P(BUILTIN_FUNC_ADD,     "+",    2, params_binary_int,   0,  ET_OP_IADD) \
    P(BUILTIN_FUNC_SUB,     "-",    2, params_binary_int,   1,  ET_OP_ISUB) \
    P(BUILTIN_FUNC_DIV,     "/",    2, params_binary_int,   2,  ET_OP_IDIV) \
    P(BUILTIN_FUNC_MUL,     "*",    2, params_binary_int,   3,  ET_OP_IMUL) \
    P(BUILTIN_FUNC_EQ,     "==",    2, params_binary_bool,  4,  ET_OP_EQUAL) \

struct __func_t;

enum __expr_type_t;

struct __func_t * neart_builtin_func_lookup(enum __expr_type_t type);

#endif /* _TYPES_H_ */
