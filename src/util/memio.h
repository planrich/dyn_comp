#ifndef MEMIO_H
#define MEMIO_H

typedef struct __memio_h {
    void * memory;
    int32_t cursor;
    int32_t size;
} memio_t;

memio_t * neart_alloc_memio(void);

void mioo_byte(memio_t * io, char byte);
char mioi_byte(memio_t * io);

void mioo_int_le(memio_t * io, int32_t integer);
int32_t mioi_int_le(memio_t * io);



#endif
