#ifndef _CODE_H_
#define _CODE_H_

#include <stdio.h>
#include <inttypes.h>
#include "gpir.h"
#include "cpool.h"

#define NEART_MAGIC (0x7472656e)

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

typedef struct __rcode_header_t {
    uint32_t magic; 
    uint8_t version;
    uint32_t cpool_offset;
    uint32_t cpool_data_offset;
    uint32_t cpool_length;
    uint32_t code_offset;
    uint32_t code_length;
} rcode_header_t;


#endif /* _CODE_H_ */
