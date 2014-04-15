#ifndef _CODE_H_
#define _CODE_H_

#include <stdio.h>
#include <inttypes.h>
#include "gpir.h"
#include "cpool.h"

#define NEART_MAGIC (0x7472656e)

typedef char rcode_t;

typedef struct __rcode_header_t {
    uint32_t magic; 
    uint8_t version;
    uint32_t cpool_offset;
    uint32_t cpool_data_offset;
    uint32_t cpool_length;
    uint32_t code_offset;
    uint32_t code_length;
    uint32_t main_offset;
} rcode_header_t;

/**
 * returns 0 if the instruction is not a jump,
 * a negative or positive number instead.
 */
int32_t n_rcode_jmp_offset(rcode_t * rcode);

#endif /* _CODE_H_ */
