
#include "vm.h"

#include <stdlib.h>
#include <inttypes.h>

NEART_INSTR_FORECH(INSTR_DEFINES_SHORT)

int neart_exec(code_t * code) {

    static void ** labels[] = { 
        NEART_INSTR_FORECH(INSTR_JMP_TABLE)
    };

    register_t registers[128];
    stack_t * stack = alloca(2^10);
    char p1,p2,p3,p4;

    stack_t * sp = stack;
    stack_t * bp = stack;
    register code_t * ip = code;

    code_t instr;

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
    printf("reg %d %ld 0x%x\n", p1, registers[p1], registers[p1]);
    goto vm_dispatch;
instr_end:
    return 1;
}
