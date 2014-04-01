#ifndef _CPOOL_BUILDER_H_
#define _CPOOL_BUILDER_H_

#include <inttypes.h>
#include "cpool.h"

typedef struct __cpool_builder_t {
    /**
     * contains the following: <int32_t>:<string> <--- the name
     *                         ^
     *                         |
     *                         offset in code
     */
    cpool_t * sym_pool;
    // <int32_t> code offset
    cpool_t * func_pool;
} cpool_builder_t;

int32_t neart_cpool_builder_find_or_reserve_index(cpool_builder_t * cpool, const char * name);

cpool_builder_t * neart_cpool_builder_alloc(void);

void neart_cpool_builder_free(cpool_builder_t * bld);

#endif
