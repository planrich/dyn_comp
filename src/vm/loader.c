#include "loader.h"

#include "utils.h"
#include "io.h"
#include "logging.h"
#include <stdio.h>
#include "gc.h"

vmctx_t * neart_vmctx_alloc(void) {
    GC_ALLOC_STRUCT(vmctx_t, ctx);
    ctx->code = NULL;
    ctx->symbols = NULL;

    return ctx;
}

void neart_vmctx_free(vmctx_t * ctx) {
    /*
    neart_cpool_free(ctx->symbols);
    free(ctx->code);
    free(ctx);
    */
}

rcode_header_t _read_header(IO * io) {
    rcode_header_t header;
    // TODO what about byte aligning? different platforms -> big/little endian
    fread(&header, sizeof(rcode_header_t), 1, io);
    return header;
}

cpool_t * _load_constant_pool(IO * io, rcode_header_t * header) {
    fseek(io, header->cpool_offset, SEEK_SET);
    GC_ALLOC_STRUCT(cpool_t, pool);

    uint32_t index_length = header->cpool_data_offset - header->cpool_offset;
    fseek(io, header->cpool_offset, SEEK_SET);
    void * index = GC_MALLOC(index_length);
    fread(index, header->code_length, 1, io);

    pool->size = index_length / 4;
    pool->offset_start = index;
    pool->offset_end = index + index_length;

    uint32_t data_length = header->cpool_length - index_length;
    fseek(io, header->cpool_data_offset, SEEK_SET);
    void * data = GC_MALLOC(data_length);
    fread(data, data_length, 1, io);

    pool->pool_start = data;
    pool->pool_end = data + data_length;

    return pool;
}

rcode_t * _load_code(IO * io, rcode_header_t * header) {
    fseek(io, header->code_offset, SEEK_SET);
    rcode_t * code = GC_MALLOC(header->code_length);
    fread(code, header->code_length, 1, io);
    return code;
}

vmctx_t * neart_load_rcode_file(const char * file) {

    FILE * in = fopen(file, "r");

    vmctx_t * ctx = neart_vmctx_alloc();

    rcode_header_t header = _read_header(in);
    if (header.magic != NEART_MAGIC) {
        NEART_LOG_FATAL("magic does not match\n");
    }

    ctx->symbols = _load_constant_pool(in, &header);

    ctx->code = _load_code(in, &header);
    ctx->main_offset = header.main_offset;

    return ctx;
}
