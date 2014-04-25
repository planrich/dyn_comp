
#include "memio.h"
#include "util.h"
#include "gc.h"

void _ensure_size(memio_t * io, int new) {
    if (io->cursor + new >= io->size) {
        io->size *= 2;
        io->memory = GC_REALLOC(io->memory, io->size);
    }
}

memio_t * neart_alloc_memio(void) {
    GC_ALLOC_STRUCT(memio_t, memio);
    memio->memory = GC_MALLOC(64);
    memio->size = 64;
    memio->cursor = 0;

    return memio;
}

void mem_o_byte(memio_t * io, char byte) {
    _ensure_size(io, 1);
    char * m = io->memory + io->cursor++;
    *m = byte;
}

void mem_o_int_le(memio_t * io, int32_t integer) {
    _ensure_size(io, 4);
    int32_t * m = io->memory + io->cursor;
    *m = integer; // TODO ensure little endian!
}
