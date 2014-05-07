#ifndef MEMIO_H
#define MEMIO_H

#include <inttypes.h>

typedef struct __memio_h {
    void * memory;
    int32_t cursor;
    int32_t size;
} memio_t;

memio_t * memio_alloc(void);

void mem_set_exec(memio_t * io);

void mem_o_byte(memio_t * io, char byte);

void mem_o_int_le(memio_t * io, int32_t integer);

void mem_o_long_le(memio_t * io, int64_t integer);

#endif
