#ifndef _QCODE_H_
#define _QCODE_H_

#include "code.h"

KHASH_MAP_INIT_STR(str_int, uint32_t);

#define __free(x)
KLIST_INIT(32, int32_t, __free);

struct __qinstr_t {
    rcode_t instruction;
    int32_t target;
    int32_t param1;
    int32_t param1_type;
    int32_t param2;
    int32_t param2_type;
};
typedef struct __qinstr_t qinstr_t;

struct __qcode_t {
    uint32_t instr_count;
    uint32_t instr_cursor;
    qinstr_t * instr;
};
typedef struct __qcode_t qcode_t;

qcode_t * neart_generate_register_code(module_t * module);

#endif /* _QCODE_H_ */
