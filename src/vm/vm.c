
#include "vm.h"

#include <stdlib.h>
#include <inttypes.h>
#include <alloca.h>
#include "loader.h"
#include "logging.h"
#include "config.h"

#define NOT_IMPL 0xff

#ifdef NEART_DEBUG
    #define VM_LOG(string, ...) printf(string, ##__VA_ARGS__)
#else
    #define VM_LOG(string, ...)
#endif

int neart_exec(vmctx_t * ctx) {

    static void ** labels[] = { 
        NEART_INSTR_FORECH(INSTR_JMP_TABLE)
    };

    int32_t i1;
    char p1,p2,p3,p4;

    stack_cell_t * stack = ctx->stack;
    register_t * registers = ctx->registers;
    stack_cell_t * sp = stack;
    stack_cell_t * bp = stack;
    rcode_t * code_base = ctx->code;
    register rcode_t * ip = ctx->code;
    rcode_t instr;

vm_dispatch:
    VM_LOG("dispatch opcode 0x%x %p\n", *ip, ip);
    instr = *ip++;
    goto *labels[instr];

instr_int_stack_push:
    *sp++ = *((int32_t*)ip);
    ip += 4;
    goto vm_dispatch;

instr_reg_sub:
    p1 = *ip++;
    p2 = *ip++;
    p3 = *ip++;
    registers[p3] = registers[p1] - registers[p2];
    goto vm_dispatch;

instr_reg_add:
    p1 = *ip++;
    p2 = *ip++;
    p3 = *ip++;
    registers[p3] = registers[p1] + registers[p2];
    goto vm_dispatch;

instr_int_load_stack:
    p1 = *ip++;
    sp--;
    registers[p1] = *((int32_t*)sp);
    goto vm_dispatch;

instr_reg_print:
    p1 = *ip++;
    printf("reg %d %lld 0x%llx\n", p1, registers[p1], registers[p1]);
    goto vm_dispatch;
instr_reg_load_int32: // 0x7

    p1 = *ip++;

    registers[p1] = *((int32_t*)ip);
    ip += 4;

    goto vm_dispatch;
instr_call: // 0x6
    i1 = *((int32_t*)ip);
    ip += 4;

    // push the base pointer
    *sp = (stack_cell_t)bp;
    //
    bp = sp;
    sp -= 1;

    // push the return address
    *sp = (stack_cell_t)ip;
    sp -= 1;

    // TODO local variables -> reserve on the stack

    ip = code_base + i1;
    goto vm_dispatch;
instr_enter: // 0x8
    // first parameter points into cpool
    ip += 4;
    goto vm_dispatch;
instr_ret:
    sp = bp - 1;
    ip = (rcode_t*)*sp;

    sp += 1;
    bp = (stack_cell_t*)*sp;

    if (bp == stack) {
        return 0;
    }

    goto vm_dispatch;

}
