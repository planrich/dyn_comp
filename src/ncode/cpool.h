#ifndef _CPOOL_H_
#define _CPOOL_H_

#include <inttypes.h>

#define neart_cpool_size(pool) *((uint32_t*) pool)
#define neart_cpool_get_offset(pool,idx)  *(pool->pool_offsets + (uint32_t)idx);

typedef struct __cpool_t {
    uint32_t size;
    uint32_t offset;
    uint32_t * offset_start;
    uint32_t * offset_end;
    uint32_t * offset_cursor;
    uint32_t * pool_start;
    uint32_t * pool_end;
    uint32_t * pool_cursor;
} cpool_t;

cpool_t * neart_cpool_alloc();

uint32_t neart_cpool_insert(cpool_t * pool, void * data, int offset, int size);

void neart_cpool_free(cpool_t * pool);

#define neart_cpool_insert_int(pool, integer) neart_cpool_insert(pool, &integer, 0, 4)
#define neart_cpool_insert_str(pool, str) neart_cpool_insert(pool, &str, 0, strlen(str))

#endif
