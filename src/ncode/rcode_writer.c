#include "qcode.h"

#include <stdio.h>
#include "cpool.h"
#include "logging.h"
#include "io.h"
#include "rcode.h"
#include "vm.h"

static void fwrite_cpool(FILE * out, cpool_t * pool) {
    uint32_t size = neart_cpool_total_size(pool);

    size = neart_cpool_offset_size(pool);
    fwrite(pool->offset_start, size, 1, out);

    size = neart_cpool_pool_size(pool);
    fwrite(pool->pool_start, size, 1, out);
}

static int32_t fwrite_rcode(IO * io, qcode_t * code, cpool_t * pool) {

    klist_t(32) * addresses = kl_init(32);

    uint32_t code_base = (uint32_t)ftell(io);

    int32_t offset = 0;
    int i = 0;
    char type = '\0';
    qinstr_t * instr = code->instr;
    while (i < code->instr_cursor) {
        ioo_byte(io, instr->instruction);
        int32_t uses = neart_parameter_usage(instr->instruction);
        int32_t p1 = instr->param1;
        int32_t p2 = instr->param2;

        if (instr->instruction == N_ENTER) {
            void * data = neart_cpool_lookup(pool, p1);
            // since opcode is already written substract 1
            uint32_t current = ftell(io);
            *((uint32_t*)data) = (current - code_base) - 1; 
        }

        if (instr->instruction == N_CALL) {
            *kl_pushp(32, addresses) = ftell(io); 
            *kl_pushp(32, addresses) = p1;
        }

        offset += 1;

        // parameter number 1
        if (INSTR_USES_PARAM1(uses)) {
            if (INSTR_IS_REG_PARAM1(uses)) {
                ioo_byte(io, p1);
                offset += 1;
            } else {
                ioo_int_le(io, p1);
                offset += 4;
            }
        }
        // parameter number 2
        if (INSTR_USES_PARAM2(uses)) {
            if (INSTR_IS_REG_PARAM2(uses)) {
                ioo_byte(io, p2);
                offset += 1;
            } else {
                ioo_int_le(io, p2);
                offset += 4;
            }
        }
        // the target
        if (INSTR_USES_TARGET(uses)) {
            // target is ALWAYS a register!
            if (INSTR_IS_REG_TARGET(uses)) {
                ioo_byte(io, instr->target);
                offset += 1;
            } else {
                ioo_int_le(io, instr->target);
                offset += 4;
            }
        }

        instr++;
        i++;
    }

    // fix the relative addresses of call statements
    kliter_t(32) * it;

    for (it = kl_begin(addresses); it != kl_end(addresses); it = kl_next(it)) {
        uint32_t addr = kl_val(it);
        fseek(io, addr, SEEK_SET);
        it = kl_next(it);
        uint32_t idx = kl_val(it);
        // lookup the data in the constant pool an entry looks like <int><funcname>
        // the first is the relative address from the code_base
        uint32_t * data = neart_cpool_lookup(pool, idx);
        fseek(io, addr, SEEK_SET);
        ioo_int_le(io, *data);
    }

    kl_destroy(32, addresses);

    return offset;
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

    // set header and seek to code
    uint32_t size = neart_cpool_total_size(pool);
    header.cpool_length = neart_cpool_total_size(pool);
    header.cpool_data_offset = header.cpool_offset + neart_cpool_offset_size(pool);
    fseek(io, size, SEEK_CUR);

    // write the code
    header.code_offset = ftell(io);
    header.code_length = fwrite_rcode(io, code, builder->sym_pool);

    int32_t index = neart_cpool_builder_find_or_reserve_index(builder, "main");
    header.main_offset = *((uint32_t*)neart_cpool_lookup(builder->sym_pool, index));

    // write the header after all data has been gathered
    fseek(io, 0, SEEK_SET);
    fwrite(&header, sizeof(rcode_header_t), 1, io);

    fseek(io, header.cpool_offset, SEEK_SET);
    fwrite_cpool(io, builder->sym_pool);
    NEART_LOG_DEBUG("file stats: pool %d bytes, code %d bytes\n", size, header.code_length);

    fclose(io);
}
