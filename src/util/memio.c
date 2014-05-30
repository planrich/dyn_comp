
#include "memio.h"
#include "utils.h"
#include <gc.h>
#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#ifdef LINUX
#include <malloc.h>
#endif

void _ensure_size(memio_t * io, int new) {
    if (io->cursor + new >= io->size) {
        printf("to many bytes needed for jitting\n");
        exit(EXIT_FAILURE);
        //io->size *= 2;
        //io->memory = GC_REALLOC(io->memory, io->size);
    }
}

memio_t * memio_alloc(void) {
    GC_ALLOC_STRUCT(memio_t, memio);
    memio->memory = valloc(4096);
    memio->size = 4096;
    memio->cursor = 0;

    /*
    if (posix_memalign(&(memio->memory), 4096, memio->size)) {
        fprintf(stderr, "could not align memory. errno %d\n", errno);
        exit(EXIT_FAILURE);
    }*/

    return memio;
}

void mem_set_exec(memio_t * io) {
    // unix specific

    errno = 0;
    mprotect(io->memory, io->size, PROT_READ | PROT_WRITE | PROT_EXEC);

    if (errno != 0) {
        fprintf(stderr, "could not mprotect page correctly. errno %d\n", errno == EINVAL);
        exit(EXIT_FAILURE);
    }
}

void mem_o_byte(memio_t * io, char byte) {
    _ensure_size(io, 1);
    char * m = ((char*)io->memory) + io->cursor++;
    *m = byte;
}

void mem_o_int_le(memio_t * io, int32_t integer) {
    _ensure_size(io, 4);
    char * m = io->memory + io->cursor;
    *m++ = (integer & 0xff);
    *m++ = ((integer >> 8) & 0xff);
    *m++ = ((integer >> 16) & 0xff);
    *m++ = ((integer >> 24) & 0xff);
    io->cursor += 4;
}

void mem_o_long_le(memio_t * io, int64_t integer) {

    _ensure_size(io, 8);
    char * m = io->memory + io->cursor;
    *m++ = (integer & 0xff);
    *m++ = ((integer >> 8) & 0xff);
    *m++ = ((integer >> 16) & 0xff);
    *m++ = ((integer >> 24) & 0xff);
    *m++ = ((integer >> 32) & 0xff);
    *m++ = ((integer >> 40) & 0xff);
    *m++ = ((integer >> 48) & 0xff);
    *m++ = ((integer >> 56) & 0xff);
    io->cursor += 8;

}
