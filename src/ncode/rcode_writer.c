#include "qcode.h"

#include <stdio.h>
#include "cpool.h"
#include "logging.h"
#include "io.h"
#include "rcode.h"
#include "vm.h"

int neart_parameter_usage(rcode_t instr);

static void fwrite_cpool(FILE * out, cpool_t * pool) {
    uint32_t size = neart_cpool_total_size(pool);
    NEART_LOG_DEBUG("writing pool of size %d\n", size);

    size = neart_cpool_offset_size(pool);
    fwrite(pool->offset_start, size, 1, out);

    size = neart_cpool_pool_size(pool);
    fwrite(pool->pool_start, size, 1, out);
}

static uint32_t fwrite_rcode(IO * io, qcode_t * code) {

    uint32_t size;
    int i = 0;
    char type = '\0';
    qinstr_t * instr = code->instr;
    while (i < code->instr_cursor) {
        ioo_byte(io, instr->instruction);
        int32_t uses = neart_parameter_usage(instr->instruction);
        int32_t p1 = instr->param1;
        int32_t p2 = instr->param2;

        if (instr->instruction == N_CALL) {
            // XXX resolve the address. now or later!
        }

        size += 1;

        // parameter number 1
        if (INSTR_USES_PARAM1(uses)) {
            if (INSTR_IS_REG_PARAM1(uses)) {
                ioo_byte(io, p1);
                size += 1;
            } else {
                ioo_int_le(io, p1);
                size += 4;
            }
        }
        // parameter number 2
        if (INSTR_USES_PARAM2(uses)) {
            if (INSTR_IS_REG_PARAM2(uses)) {
                ioo_byte(io, p2);
                size += 1;
            } else {
                ioo_int_le(io, p2);
                size += 4;
            }
        }
        // the target
        if (INSTR_USES_TARGET(uses)) {
            // target is ALWAYS a register!
            ioo_byte(io, instr->target);
            size += 1;
        }

        instr++;
        i++;
    }

    return size;
}

void neart_write_to_file(cpool_builder_t * builder, qcode_t * code, const char * name) {

    IO * io = fopen(name, "wb");
    if (io == NULL) {
        NEART_LOG_FATAL("faild to open file %s\n", name);
    }

    cpool_t * pool = builder->sym_pool;

    rcode_header_t header;
    header.magic = NEART_MAGIC;
    header.version = 0x01;

    // skip over header
    fseek(io, sizeof(rcode_header_t), SEEK_CUR);
    // note the cpool offset
    header.cpool_offset = ftell(io);
    printf("%x\n", ftell(io));

    // write the constant pool
    fwrite_cpool(io, builder->sym_pool);
    header.cpool_length = neart_cpool_total_size(pool);
    header.cpool_data_offset = header.cpool_offset + neart_cpool_offset_size(pool);

    // write the code
    header.code_offset = ftell(io);
    header.code_length = fwrite_rcode(io, code);

    // write the header after all data has been gathered
    fseek(io, 0, SEEK_SET);
    fwrite(&header, sizeof(rcode_header_t), 1, io);

    fclose(io);
}

#define INSTR_PARAM_TYPE(p1,p2,mask, ...) mask,
int neart_parameter_usage(rcode_t instr) {
    static char param_type[] = { NEART_INSTR_FORECH(INSTR_PARAM_TYPE) };

    return param_type[instr];
}
