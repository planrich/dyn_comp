#include "qcode.h"

#include <stdio.h>
#include "cpool.h"
#include "logging.h"

static void fwrite_cpool(FILE * out, cpool_t * pool) {
    uint32_t size = neart_cpool_total_size(pool);

    fwrite(&size, 4, 1, out);

    NEART_LOG_DEBUG("writing pool of size %d\n", size);

    // XXX
    size = neart_cpool_offset_size(pool);
    fwrite(&size, 4, 1, out);
    fwrite(pool->offset_start, size, 1, out);

    size = neart_cpool_pool_size(pool);
    fwrite(&size, 4, 1, out);
    fwrite(pool->pool_start, size, 1, out);
}

static void fwrite_rcode(FILE * out, qcode_t * code) {

}

void neart_write_to_file(cpool_builder_t * builder, qcode_t * code, const char * name) {

    FILE * out = fopen(name, "wb");
    if (out == NULL) {
        NEART_LOG_FATAL("faild to open file %s\n", name);
    }

    char header[] = "nrc";
    char version = 0x1;

    fwrite(header, 3, 1, out);
    fwrite(&version, 1, 1, out);

    fwrite_cpool(out, builder->sym_pool);
    fwrite_cpool(out, builder->func_pool);

    fwrite_rcode(out, code);

    fflush(out);
    fclose(out);
}

