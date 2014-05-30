#include "compiler.h"

#include "collections.h"
#include "bblock.h"
#include "vm.h"
#include "logging.h"
#include "reg_alloc.h"
#include "x86_64.h"
#include "gc.h"
#include "memio.h"

#define MCODE_SIZE 64

mcode_t * _jit_methods = NULL;
rcode_t * _register_code_base = NULL;

void * jit_switch(rcode_t * code);

mcode_t * _neart_jit_compile(rcode_t * code);
memio_t * neart_jit_template_transform(bbline_t * line, life_range_t * ranges);

int _count_stack_bytes(bblock_t * head);

void * jit_switch(rcode_t * code) {

    void * ret_addr = __builtin_return_address(0);
    //printf("%p return address got from jit_switch\n",ret_addr);
    /*
     * example generation 
     * 0x0000000101000012:	push   %r15
     * 0x0000000101000014:	push   %rdi <== move wptr here and write new callq (1)
     * 0x0000000101000015:	push   %rax
     * 0x0000000101000016:	movabs $0x100002310,%r15
     * 0x0000000101000020:	movabs $0x1000bef98,%rdi
     * 0x000000010100002a:	callq  *%r15
     * 0x000000010100002d:	mov    %rax,%r15 <== ret_addr (2)
     * 0x0000000101000030:	pop    %rax
     * 0x0000000101000031:	pop    %rdi
     * 0x0000000101000032:	callq  *%r15 <== eptr
     * 0x0000000101000035:	pop    %r15
     */
    void * wptr = ret_addr - 26; // (1)
    void * eptr = ret_addr + 3 + 1 + 1 + 1; // 3 mov, 1 pop, 1 pop
    memio_t memio = { .memory = wptr, .cursor = 0, .size = 4096 };

    mcode_t * m = _neart_jit_compile(code);
    arch_replace_jit_call(&memio, m, eptr);

    return m;
}

inline
mcode_t * _neart_jit_compile(rcode_t * code) {

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
    vreg_t r1,r2,t3;

    ra_state_t * state = arch_ra_state_new();
    state->ranges = ranges;

    memio_t * io = memio_alloc();

    bblock_t * first_block = line->first;

    int var_byte_count = _count_stack_bytes(first_block);
    var_byte_count = 0;
    arch_enter_routine(io,var_byte_count);

    klist_t(bb) * jmps = kl_init(bb);

    for (int i = 0; i < line->size; i++) {
        state->time_step = i;

        bblock_t * block = line->first + i;
        block->mcode = io->memory + io->cursor;
        switch (*block->instr) {
            case NR_L32:
                c1 = *(block->instr + 1);
                r1 = *(block->instr + 1 + 4);
                arch_load_32(io, c1, arch_ra_hwreg(state, r1));
                break;
            case N_CALL:
                c1 = *(block->instr + 1);
                arch_call(io, state, &jit_switch, _register_code_base + c1);
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
            case NR_SKIP_EQ:
                r1 = *(block->instr + 1);
                r2 = *(block->instr + 1 + 1);
                t3 = *(block->instr + 1 + 1 + 1);

                arch_cond_jmp(io, state, r1, r2, t3, *block->instr);
                *kl_pushp(bb, jmps) = block;
                printf("pushing block %p\n", block);
                break;
            case NR_JMP:
                t3 = *(block->instr + 1);

                arch_jmp(io, t3);
                *kl_pushp(bb, jmps) = block;
                break;
            case N_END:
                arch_ret(io,var_byte_count);
                break;
        }
    }

    kliter_t(bb) *it;
    for (it = kl_begin(jmps); it != kl_end(jmps); it = kl_next(it)) {
        bblock_t * block = kl_val(it);
        memio_t memio = { .memory = block->mcode, .cursor = 0, .size = 4096 };
        // every bb can only have 1 out edge.
        bblock_t * target = kl_val(kl_begin(block->oedges));
        arch_patch_jmp(&memio, *block->instr, target->mcode);
    }

    kl_destroy(bb, jmps);

    return io;
}

mcode_t * neart_jit_compile(vmctx_t * vmc, rcode_t * code) {

    rcode_t * base = vmc->code;

    if (_jit_methods == NULL) {
        _register_code_base = base;
        NEART_LOG_DEBUG("alloc jit methods table of size %d\n", vmc->symbols->size);
        _jit_methods = GC_MALLOC(sizeof(void*) * vmc->symbols->size);
    }

    return _neart_jit_compile(code);
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
