#include "cpool.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"

cpool_t * neart_cpool_alloc() {
    ALLOC_STRUCT(cpool_t, p);
    p->size = 0;
    p->offset = 0;
    p->offset_start = malloc(128);
    p->offset_end = p->offset_start + (128);
    p->offset_cursor = p->offset_start;

    p->pool_start = malloc(256);
    p->pool_end = p->pool_start + 256;
    p->pool_cursor = p->pool_start;

    return p;
}

uint32_t neart_cpool_insert(cpool_t * pool, void * data, int offset, int size) {

    uint32_t idx = pool->size++;
    uint32_t * off_cur = pool->offset_cursor;
    uint32_t * off_end = pool->offset_end;
    uint32_t * pool_cur = pool->pool_cursor;
    uint32_t * pool_end = pool->pool_end;

    // out of index space?
    if (off_cur + 1 <= off_end) {
        uint32_t size = (pool->offset_end - pool->offset_start) + 64;
        pool->offset_start = realloc(pool->offset_start, size);
        pool->offset_end = pool->offset_start + size;
    }

    *off_cur = pool->offset++;
    pool->offset_cursor++;

    if (pool_cur + size <= pool_end) {
        uint32_t size = (pool->pool_end - pool->pool_start) + size + 256;
        pool->pool_start = realloc(pool->pool_start, size);
        pool->pool_end = pool->pool_start + size;
    }

    memcpy(pool_cur, data + offset, size);
    pool->pool_cursor += size;

    return *off_cur;
}

void neart_cpool_free(cpool_t * p) {
    free(p->offset_start);
    free(p->offset_end);
    free(p);
}
