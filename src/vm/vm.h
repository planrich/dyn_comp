

#ifndef _VM_H_
#define _VM_H_

#include "code.h"
#include "cpool.h"

#define NEART_INSTR_FORECH(FOR_EACH) \
    FOR_EACH(0x0, N_END,   _,                  instr_end, "end the execution") \
    FOR_EACH(0x1, NI_LSI,  NI_LOAD_STACK_INT,  instr_int_load_stack, "load a 32 bit integer off the stack into the register x") \
    FOR_EACH(0x2, NR_ADD,  _,                  instr_reg_add, "add to registers into a target register (32 bit)") \
    FOR_EACH(0x3, NR_SUB,  _,                  instr_reg_sub, "add to registers into a target register (32 bit)") \
    FOR_EACH(0x4, NI_SPI,  NI_STACK_PUSH_INT,  instr_int_stack_push, "push a 32 bit integer onto the stack") \
    FOR_EACH(0x5, NR_PUT,  NR_PRINT_REGISTER,  instr_reg_print, "print the contents of a register") \
    FOR_EACH(0x6, NR_CALL, _,       instr_call, "call a function") \
    FOR_EACH(0x7, NR_L32, _,        instr_register_load_int32, "call a function") \


#define INSTR_JMP_TABLE(p1, p2, p3, label, p5, ...) &&label,
#define INSTR_ENUM_DEFINE(p1, p2, ...) p2 = p1,

typedef enum __neart_instr_codes_t {
    NEART_INSTR_FORECH(INSTR_ENUM_DEFINE)
} neart_instr_codes_t;

//const char * ncode_names[255];

typedef int64_t register_t;
typedef int32_t stack_cell_t;

int neart_exec(ncode_t * code);

#endif /* _VM_H_ */
