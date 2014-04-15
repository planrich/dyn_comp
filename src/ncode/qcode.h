#ifndef _QCODE_H_
#define _QCODE_H_

#include "rcode.h"
#include "cpool_builder.h"
#include "collections.h"

#define PT_REG (1)
#define PT_CPOOL_IDX (2)
#define PT_CONSTANT (3)

#define UNUSED (-1)

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

qcode_t * neart_generate_register_code(module_t * module, cpool_builder_t * builder);

void neart_write_to_file(cpool_builder_t * builder, qcode_t * code, const char * name);

uint32_t n_ncode_bytes(qcode_t * code);

#endif /* _QCODE_H_ */
