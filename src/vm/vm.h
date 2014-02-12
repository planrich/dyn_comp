

#ifndef _VM_H_
#define _VM_H_

#include "code.h"
#include "cpool.h"

typedef char code_t;

#define NEART_INSTR_FORECH(F) \
    F(0x0, NI_SPI,  NI_STACK_PUSH_INT,  instr_int_stack_push,\
      "push a 32 bit integer onto the stack") \
    \
    F(0x1, NI_LSI,  NI_LOAD_STACK_INT,  instr_int_load_stack,\
      "load a 32 bit integer into the register x") \
    \
    F(0x2, NR_ADD,  _,                  instr_reg_add,\
      "add to registers into a target register (32 bit)") \
    \
    F(0x3, NR_SUB,  _,                  instr_reg_sub, \
      "add to registers into a target register (32 bit)") \
    \
    F(0x4, N_END,   _,                  instr_end,\
      "end the execution") \
    \
    F(0x5, NR_PUT,  NR_PRINT_REGISTER,  instr_reg_print,\
      "print the contents of a register") \
    \

#define INSTR_JMP_TABLE(p1, p2, p3, label, p5, ...) &&label,
#define INSTR_DECLARE_SHORT(p1, p2, ...) const code_t p2;
#define INSTR_DEFINES_SHORT(p1, p2, ...) const code_t p2 = ((code_t)p1);

NEART_INSTR_FORECH(INSTR_DECLARE_SHORT)

typedef int64_t register_t;
typedef int32_t stack_t;

int neart_exec(code_t * code);

#endif /* _VM_H_ */
