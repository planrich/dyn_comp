

#ifndef _VM_H_
#define _VM_H_

#include "rcode.h"
#include "cpool.h"

#define INSTR_IS_REG_PARAM1(integer) ((integer & 0x10) != 0)
#define INSTR_IS_REG_PARAM2(integer) ((integer & 0x8) != 0)
#define INSTR_USES_PARAM1(integer) ((integer & 0x4) != 0)
#define INSTR_USES_PARAM2(integer) ((integer & 0x2) != 0)
#define INSTR_USES_TARGET(integer) ((integer & 0x1) != 0)

/**
 * Respect the order of instructions. 
 */
#define NEART_INSTR_FORECH(FOR_EACH) \
    FOR_EACH(0x0, N_END,    0b00000, instr_ret,                 "return from this function. pops the stack frame") \
    FOR_EACH(0x1, NI_LSI,   0b10101, instr_int_load_stack,      "load a 32 bit integer off the stack into the register x") \
    FOR_EACH(0x2, NR_ADD,   0b11111, instr_reg_add,             "add to registers into a target register (32 bit)") \
    FOR_EACH(0x3, NR_SUB,   0b11111, instr_reg_sub,             "sub to registers into a target register (32 bit)") \
    FOR_EACH(0x4, NI_SPI,   0b00100, instr_int_stack_push,      "push a 32 bit integer onto the stack") \
    FOR_EACH(0x5, NR_PUT,   0b10100, instr_reg_print,           "print the contents of a register") \
    FOR_EACH(0x6, N_CALL,   0b00100, instr_call,                "call a function") \
    FOR_EACH(0x7, NR_L32,   0b00101, instr_reg_load_int32,      "load an int32") \
    FOR_EACH(0x8, N_ENTER,  0b00100, instr_enter,               "the beginning of a method") \
    FOR_EACH(0x9, NR_MOV,   0b10101, instr_reg_mov,             "move the value of a register to another register") \
  //FOR_EACH(0xa, N_TCALL,  0b00100, instr_tail_call,           "call a function again and override the stack frame") \

/*                            +++++      
 *                            ||||+-> target
 *                            |||+--> param2
 *                            ||+---> param1
 *                            |+----> param2 is register
 *                            +-----> param1 is register
 *
 * target type is always a register
 */


#define INSTR_JMP_TABLE(p1, p2, p3, label, p5, ...) &&label,
#define INSTR_ENUM_DEFINE(p1, p2, ...) p2 = p1,

typedef enum __neart_instr_codes_t {
    NEART_INSTR_FORECH(INSTR_ENUM_DEFINE)
} neart_instr_codes_t;

//const char * ncode_names[255];

typedef int64_t register_t;
typedef int32_t stack_cell_t;

struct __vmctx_t;

int neart_exec(struct __vmctx_t * code);

int neart_parameter_usage(rcode_t instr);

#endif /* _VM_H_ */
