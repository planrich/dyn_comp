#ifndef _CPOOL_H_
#define _CPOOL_H_

#include <inttypes.h>

#define neart_cpool_size(pool) *((uint32_t*) pool)
#define neart_cpool_get_offset(pool,idx)  *(pool->pool_offsets + (uint32_t)idx);


/**
 *     offset (4 byte each)         data
 *  ------------------   ------------------------------
 * /                  \ /
 * +----------------------------------------------------+
 * | 16 | 30 | .. | .. |    funcname.|    main.|
 * +--+----+--------------------------------------------+
 *    |    |            ^             ^
 *    +----|------------+             |
 *         +--------------------------+
 */
typedef struct __cpool_t {
    uint32_t * offset_start;
    uint32_t * offset_end;
    void * pool_start;
    void * pool_end;

    uint32_t size; // the actual size
    uint32_t offset; // the offset. mainly for book keeping (could be calc out of offset_start and offset_cursor
    uint32_t * offset_cursor;
    void * pool_cursor;
} cpool_t;

cpool_t * neart_cpool_alloc();

uint32_t neart_cpool_insert(cpool_t * pool, void * data, int offset, int size);

void neart_cpool_free(cpool_t * pool);

/**
 * aquire space in the constant pool. the index parameter can be NULL,
 * if it is not the index in the offset will be returned
 */
void * neart_cpool_reserve(cpool_t * pool, int32_t size, int32_t * index);

#define neart_cpool_insert_int(pool, integer) neart_cpool_insert(pool, &integer, 0, 4)
#define neart_cpool_insert_str(pool, str) neart_cpool_insert(pool, &str, 0, strlen(str))

uint32_t neart_cpool_total_size(cpool_t * pool);
uint32_t neart_cpool_offset_size(cpool_t * pool);
uint32_t neart_cpool_pool_size(cpool_t * pool);

#endif
