#ifndef _CODE_H_
#define _CODE_H_

#include <stdio.h>
#include <inttypes.h>
#include "gpir.h"
#include "cpool.h"

// cpi is constant pool index

typedef char rcode_t;

typedef struct __code_t {
    uint32_t func_cpi;
    uint32_t code_length;
} code_t;

typedef struct __ncode_t {
    cpool_t * cpool;
    code_t * code;
} ncode_t;

ncode_t * neart_generate_register_code(module_t * module);

#endif /* _CODE_H_ */
