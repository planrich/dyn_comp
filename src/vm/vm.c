
#include "vm.h"

#include <stdlib.h>
#include <inttypes.h>
#include "loader.h"
#include "logging.h"
#include "config.h"

#define NOT_IMPL 0xff

#ifdef NEART_DEBUG
    #define VM_LOG(string, ...) printf(string, ##__VA_ARGS__)
#else
    #define VM_LOG(string, ...)
#endif

#define stack_push_64(var) *((int64_t*)sp) = var; sp -= 2
#define stack_push_32(var) *((int32_t*)sp) = var; sp -= 1

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

    // TODO stack not with alloca

    stack_push_64(bp);
    bp = sp + 2;

    // push the return address
    stack_push_32(code_base);
    VM_LOG("push return address %p\n", ip - code_base);

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
    VM_LOG("call ip is %p\n", ip);

    i1 = *((int32_t*)ip);
    ip += 4;

    // push the base pointer
    stack_push_64(bp);
    bp = sp + 2;

    // push the return address
    stack_push_32(ip - code_base);
    VM_LOG("push return address %p\n", ip - code_base);

    // TODO local variables -> reserve on the stack

    ip = code_base + i1;
    VM_LOG("call ip now is is %p ant points to %x\n", ip, *ip);
    VM_LOG("bp is %p sp is %p\n", bp, sp);
    goto vm_dispatch;
instr_enter: // 0x8
    // first parameter points into cpool
    ip += 4;
    goto vm_dispatch;
instr_ret:
    sp = bp - 2;
    ip = code_base + *sp;
    VM_LOG("call ip now is is %p ant points to %x %p\n", ip, *ip, sp);
    return 0;

    sp = bp;
    bp = *((uint64_t*)sp);

    printf("stack %p bp %p sp %p\n", stack, bp, sp);
    if (sp == stack) {
        return 0;
    }

    goto vm_dispatch;

}
