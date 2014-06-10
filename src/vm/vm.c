
#include "vm.h"

#include <stdlib.h>
#include <inttypes.h>
#include "loader.h"
#include "logging.h"
#include "config.h"

#define NOT_IMPL 0xff

#ifdef NEART_DEBUG
    #define VM_LOG(string, ...) NEART_LOG_DEBUG(string, ##__VA_ARGS__)
#else
    #define VM_LOG(string, ...)
#endif

#define stack_push_64(var) *((int64_t*)sp) = ((int64_t)var); sp -= 2
#define stack_pop_64(var,type) sp += 2; var = (type*)*((int64_t*)sp)
#define stack_push_32(var) *sp = var; sp -= 1
#define stack_pop_32(var) sp += 1; var = *sp

void * get_sp() {
    void * ptr;
    __asm__( "movq %%rsp, %0" : "=r"(ptr));
    return ptr;
}

int neart_exec(vmctx_t * ctx) {

    static void ** labels[] = { 
        NEART_INSTR_FORECH(INSTR_JMP_TABLE)
    };

    int32_t i1;
    char p1,p2,p3,p4;
    int v;

    stack_cell_t * stack;// = (stack_cell_t*)get_stack_pointer();
    register_t * registers = ctx->registers;
    stack_cell_t * sp; // = stack;
    stack_cell_t * bp; // = stack;
    rcode_t * code_base = ctx->code;
    register rcode_t * ip = code_base + ctx->main_offset;
    rcode_t instr;

    stack = get_sp();
    ctx->stack = &stack;

    stack -= 1000; // TODO after using get_sp() the stack grows later?
    sp = stack;
    bp = stack;

    stack_push_64(bp);
    bp = sp;

    stack = sp;
    VM_LOG("top of stack %p\n", stack);

    stack_push_64(0); // pseudo return addr

vm_dispatch:
    VM_LOG("dispatch opcode 0x%x %p byteoffset: %d\n", *ip, ip, (int)(ip - code_base));
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
    goto vm_dispatch;
instr_reg_load_int32: // 0x7
    p1 = *(ip+4);

    registers[p1] = *((int32_t*)ip);
    ip += 5;

    goto vm_dispatch;
instr_call: // 0x6
    i1 = *((int32_t*)ip);
    ip += 4;

    // push the return address
    stack_push_64(ip);

    ip = code_base + i1;
    goto vm_dispatch;
instr_enter: // 0x8
    // first parameter points into cpool
    ip += 4;
    p2 = *ip++; // how many local variables
    p1 = *ip++; // how many registers from [7..255)
    VM_LOG("enter sp: %p\n", sp);

    stack_push_64(bp);
    bp = sp;

    for (v = 0; v < p2; v++) {
        stack_push_64(registers[v]);
    }

    for (v = 0; v < p1; v++) {
        stack_push_64(registers[v+7]);
    }
    stack_push_32(p1);

    goto vm_dispatch;
instr_ret: //0x0

    stack_pop_32(p1);
    for (v = p1-1; v >= 0; v--) {
        stack_pop_64(registers[v+7], register_t);
    }
    sp = bp;
    stack_pop_64(bp, stack_cell_t);

    VM_LOG("exit sp: %p\n", sp);

    stack_pop_64(ip, rcode_t);
    //sp = bp - 2;
    //ip = *sp;
    VM_LOG("sp after popping: %p\n", sp);

    //sp = bp;
    //bp = *((stack_cell_t**)sp);

    if (sp == stack) {
        VM_LOG("reached end of program. ret sp = stack\n");
        for (int i = 0; i < 10; i++) {
            VM_LOG("REG %d: value %lld (0x%llx)\n", i, registers[i], registers[i]);
        }
        return 0;
    }

    goto vm_dispatch;
instr_reg_mov: // 0x9
    p1 = *ip++;
    p2 = *ip++;
    if (p1 < 6) {
        registers[p2] = *(bp - (p1*2));
    } else {
        registers[p2] = registers[p1];
    }
    goto vm_dispatch;

instr_jmp: // 0xa  -> the short jump [-127,128]

    p1 = *ip;

    VM_LOG("jumping from %p to %p", ip, ip + p1 - 1);
    // ip is on the parameter p1 -> move one back -1 and then change ip
    ip = ip + p1 - 1;

    goto vm_dispatch;
instr_skip_equal: // 0xb
    p1 = *ip++;
    p2 = *ip++;
    p3 = *ip++;
    if (registers[p1] == registers[p2]) {
        ip += p3;
        VM_LOG("jumping %d bytes\n", p3);
    }
    goto vm_dispatch;
instr_meth_bound:
    VM_LOG("dispatching method bound. exiting\n");
    return EXIT_FAILURE;
instr_stack_arg:
    p1 = *ip++;
    // TODO load p1 from stack...
    goto vm_dispatch;
instr_push_reg:
    p1 = *ip++;
    // TODO push on the stack
    goto vm_dispatch;
instr_skip_not_equal: // 0xf
    p1 = *ip++;
    p2 = *ip++;
    p3 = *ip++;
    if (registers[p1] != registers[p2]) {
        ip += p3;
        VM_LOG("jumping %d bytes\n", p3);
    }
    goto vm_dispatch;
instr_skip_less_equal: // 0x10
    p1 = *ip++;
    p2 = *ip++;
    p3 = *ip++;
    if (registers[p1] <= registers[p2]) {
        ip += p3;
        VM_LOG("jumping %d bytes\n", p3);
    }
    goto vm_dispatch;
instr_skip_greater_equal: // 0x11
    p1 = *ip++;
    p2 = *ip++;
    p3 = *ip++;
    if (registers[p1] >= registers[p2]) {
        ip += p3;
        VM_LOG("jumping %d bytes\n", p3);
    }
    goto vm_dispatch;
instr_reg_mul: // 0x12
    p1 = *ip++;
    p2 = *ip++;
    p3 = *ip++;
    registers[p3] = registers[p1] * registers[p2];
    goto vm_dispatch;
instr_reg_div: // 0x13
    p1 = *ip++;
    p2 = *ip++;
    p3 = *ip++;
    registers[p3] = registers[p1] / registers[p2];
    goto vm_dispatch;
}
