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

mcode_t * _neart_jit_compile(rcode_t * base, rcode_t * code);
memio_t * neart_jit_template_transform(rcode_t * base, bbline_t * line, life_range_t * ranges);

int _count_stack_bytes(bblock_t * head);

mcode_t * _neart_jit_compile(rcode_t * base, rcode_t * code) {

    printf("starting to compile %p\n", code);
    bbline_t * line = neart_bbnize(code);

    // calculate life ranges
    life_range_t * life_ranges = neart_ra_life_ranges(line);

    // then invoke the assembler
    memio_t * io = neart_jit_template_transform(base, line, life_ranges);

    mem_set_exec(io);

    return io->memory;
}

memio_t * neart_jit_template_transform(rcode_t * base, bbline_t * line, life_range_t * ranges) {

    int mcode_size = MCODE_SIZE;
    int32_t c1,c2;
    vreg_t r1,r2,t3;

    ra_state_t * state = arch_ra_state_new();
    state->ranges = ranges;

    memio_t * io = memio_alloc();

    bblock_t * first_block = line->first;

    int var_byte_count = _count_stack_bytes(first_block);
    var_byte_count = 4;
    arch_enter_routine(io,var_byte_count);

    for (int i = 0; i < line->size; i++) {
        state->time_step = 0;

        bblock_t * block = line->first + i;
        int time_step = i;
        switch (*block->instr) {
            case NR_L32:
                c1 = *(block->instr + 1);
                r1 = *(block->instr + 1 + 4);
                arch_load_32(io, c1, arch_ra_hwreg(state, ranges, r1, time_step));
                //if (r1 < 6) {
                    // this is a paramter for a call -> push it on the stack
                    //arch_push_const(io, c1);
                //} else {
                //}
                break;
            case N_CALL:
                c1 = *(block->instr + 1);
                printf("_neart_hit_compile is at %p\n", &_neart_jit_compile);
                arch_call(io, state, &_neart_jit_compile, base, base + c1, i);
                break;
            case NR_MOV:
                r1 = *(block->instr + 1);
                t3 = *(block->instr + 1 + 1);
                arch_move_reg(io, state, r1, t3);
                break;
            case NR_ADD:
                r1 = *(block->instr + 1);
                r2 = *(block->instr + 1 + 1);
                t3 = *(block->instr + 1 + 1 + 1);
                arch_add_reg(io, state, r1, r2, t3);
                break;
            case N_END:
                arch_ret(io,var_byte_count);
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

mcode_t * neart_jit_compile(vmctx_t * vmc, rcode_t * code) {

    rcode_t * base = vmc->code;

    if (_jit_methods == NULL) {
        NEART_LOG_DEBUG("alloc jit methods table of size %d\n", vmc->symbols->size);
        _jit_methods = GC_MALLOC(sizeof(void*) * vmc->symbols->size);
    }

    return _neart_jit_compile(base, code);
}

int _count_stack_bytes(bblock_t * head) {
    int count = 0;

    bblock_t * wptr = head;
    //rcode_t * instr;
    //while (*wtpr->instr != NR_L32) {
        //if (*instr == NR_MOV
    //}

    return count;
}
