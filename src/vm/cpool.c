#include "cpool.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"

cpool_t * neart_cpool_alloc() {
    GC_ALLOC_STRUCT(cpool_t, p);
    p->size = 0;
    p->offset = 0;
    p->offset_start = GC_MALLOC(128);
    p->offset_end = p->offset_start + (128);
    p->offset_cursor = p->offset_start;

    p->pool_start = GC_MALLOC(256);
    p->pool_end = p->pool_start + 256;
    p->pool_cursor = p->pool_start;
    
    return p;
}

void * neart_cpool_lookup(cpool_t * pool, int32_t index) {
    uint32_t idx = *(pool->offset_start + (uint32_t)index);
    return pool->pool_start + idx;
}

void * neart_cpool_reserve(cpool_t * pool, int32_t size, int32_t * index) {

    void * off_cur = pool->pool_cursor;

    int32_t i = neart_cpool_insert(pool, NULL, 0, size);
    if (index != NULL) {
        *index = i;
    }

    return off_cur;
}

uint32_t neart_cpool_insert(cpool_t * pool, void * data, int offset, int size) {

    uint32_t idx = pool->offset;
    void * off_cur = pool->offset_cursor;
    void * off_end = pool->offset_end;
    void * pool_cur = pool->pool_cursor;
    void * pool_end = pool->pool_end;

    //printf("insert! os: %ld oc: %ld, oe: %ld, diff: %ld\n", pool->offset_start, off_cur, off_end, off_end - off_cur);
    //printf("insert! ps: %ld pc: %ld, pe: %ld, diff: %ld\n", pool->offset_end, pool_cur, pool_end, pool_end - pool_cur);

    // out of index space?
    if (off_cur + 1 >= off_end) {
        uint32_t size = (pool->offset_end - pool->offset_start) + 64;
        pool->offset_start = GC_REALLOC(pool->offset_start, size);
        pool->offset_end = pool->offset_start + size;
        printf("REALLOC OFFSET\n");
    }

    // set the value of the offset to cur - start = the offset in bytes
    // starting from the offset in the pool (relative)
    *((int32_t*)off_cur) = pool_cur - pool->pool_start;
    pool->offset++;
    pool->offset_cursor++;

    if (pool_cur + size >= pool_end) {
        //uint32_t size = (pool->pool_end - pool->pool_start) + size + 256;
        pool->pool_start = GC_REALLOC(pool->pool_start, size);
        pool->pool_end = pool->pool_start + size;
        printf("REALLOC POOL\n");
    }

    if (data != NULL) {
        memcpy(pool_cur, data + offset, size);
    }
    pool->pool_cursor += size;

    return idx;
}

void neart_cpool_free(cpool_t * p) {
    /*
    free(p->offset_start);
    free(p->pool_start);
    free(p);
    */
}

uint32_t neart_cpool_total_size(cpool_t * pool) {
    return neart_cpool_offset_size(pool) + neart_cpool_pool_size(pool);
}

uint32_t neart_cpool_offset_size(cpool_t * pool) {
    return ((void*)pool->offset_cursor - (void*)pool->offset_start);
}

uint32_t neart_cpool_pool_size(cpool_t * pool) {
    return pool->pool_cursor - pool->pool_start;
}
