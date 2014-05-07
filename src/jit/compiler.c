#include "compiler.h"

#include "bblock.h"
#include "vm.h"
#include "logging.h"
#include "reg_alloc.h"
#include "x86_64.h"
#include "gc.h"
#include "memio.h"

#define MCODE_SIZE 64

static mcode_t * _jit_methods = NULL;

memio_t * neart_jit_template_transform(bbline_t * line, life_range_t * ranges);

mcode_t * neart_jit_compile(vmctx_t * vmc, rcode_t * code) {

    void * machine_code = NULL;

    if (_jit_methods == NULL) {
        NEART_LOG_DEBUG("alloc jit methods table of size %d\n", vmc->symbols->size);
        _jit_methods = GC_MALLOC(sizeof(void*) * vmc->symbols->size);
    }

    bbline_t * line = neart_bbnize(code);

    // calculate life ranges
    life_range_t * life_ranges = neart_ra_life_ranges(line);

    // then invoke the assembler
    memio_t * io = neart_jit_template_transform(line, life_ranges);

    mem_set_exec(io);

    return io->memory;
}


memio_t * neart_jit_template_transform(bbline_t * line, life_range_t * ranges) {

    int mcode_size = MCODE_SIZE;
    int32_t c1,c2;
    vreg_t r1,r2,r3;

    ra_state_t state;

    memio_t * io = memio_alloc();

    for (int i = 0; i < line->size; i++) {
        bblock_t * block = line->first + i;
        switch (*block->instr) {
            case NR_L32:
                c1 = *(block->instr + 1);
                r1 = *(block->instr + 1 + 4);
                arch_load_32(io, c1, arch_ra_hwreg(&state, ranges, r1));
                break;
            case N_CALL:
                c1 = *(block->instr + 1);
                r1 = *(block->instr + 1 + 4);
                arch_load_32(io, c1, arch_ra_hwreg(&state, ranges, r1));
                break;
            case N_END:
                arch_ret(io);
                break;
        }
    }

    int size = io->cursor;
    for (int i = 0; i < size; i++) {
        NEART_LOG_INFO("0x%x ", *(((unsigned char*)io->memory)+i));
    }
    NEART_LOG_INFO("\n");

    return io;
}
