
#include "vm.h"

#include <stdlib.h>
#include <inttypes.h>
#include <alloca.h>

int neart_exec(ncode_t * code) {

    static void ** labels[] = { 
        NEART_INSTR_FORECH(INSTR_JMP_TABLE)
    };

    register_t registers[128];
    stack_cell_t * stack = alloca(2^10);
    char p1,p2,p3,p4;

    stack_cell_t * sp = stack;
    stack_cell_t * bp = stack;
    register rcode_t * ip = NULL;//code;

    rcode_t instr;

vm_dispatch:
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
instr_register_load_int32:

    goto vm_dispatch;
instr_call:


    goto vm_dispatch;
instr_end:
    return 1;
}
