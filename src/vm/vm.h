

#ifndef _VM_H_
#define _VM_H_

#include "rcode.h"
#include "cpool.h"

#define INSTR_IS_REG_TARGET(integer) ((integer & 0x20) != 0)
#define INSTR_IS_REG_PARAM1(integer) ((integer & 0x10) != 0)
#define INSTR_IS_REG_PARAM2(integer) ((integer & 0x8) != 0)
#define INSTR_USES_PARAM1(integer) ((integer & 0x4) != 0)
#define INSTR_USES_PARAM2(integer) ((integer & 0x2) != 0)
#define INSTR_USES_TARGET(integer) ((integer & 0x1) != 0)

/**
 * Respect the order of instructions. 
 */
#define NEART_INSTR_FORECH(FOR_EACH) \
    FOR_EACH(0x0,  N_END,        0b000000, instr_ret,                 "return from this function. pops the stack frame") \
    FOR_EACH(0x1,  NI_LSI,       0b110101, instr_int_load_stack,      "load a 32 bit integer off the stack into the register x") \
    FOR_EACH(0x2,  NR_ADD,       0b111111, instr_reg_add,             "add two registers into a target register (64 bit)") \
    FOR_EACH(0x3,  NR_SUB,       0b111111, instr_reg_sub,             "sub two registers into a target register (64 bit)") \
    FOR_EACH(0x4,  N_SPI,        0b000100, instr_int_stack_push,      "push a 32 bit integer onto the stack") \
    FOR_EACH(0x5,  NR_PUT,       0b010100, instr_reg_print,           "print the contents of a register") \
    FOR_EACH(0x6,  N_CALL,       0b000100, instr_call,                "call a function") \
    FOR_EACH(0x7,  NR_L32,       0b100101, instr_reg_load_int32,      "load an int32") \
    FOR_EACH(0x8,  N_ENTER,      0b101111, instr_enter,               "the beginning of a method") \
    FOR_EACH(0x9,  NR_MOV,       0b110101, instr_reg_mov,             "move the value of a register to another register") \
    FOR_EACH(0xa,  NR_JMP,       0b000001, instr_jmp,                 "move instr. pointer [-127,+128]") \
    FOR_EACH(0xb,  NR_SKIP_EQ,   0b011111, instr_skip_equal,          "if register p1 and register p2 equal skip t bytes") \
    FOR_EACH(0xc,  N_METH_BOUND, 0b000000, instr_meth_bound,          "set the bounds of a method") \
    FOR_EACH(0xd,  NR_STACK_ARG, 0b010100, instr_stack_arg,           "an argument for a routine on the stack.") \
    FOR_EACH(0xe,  NR_PUSH_REG,  0b010100, instr_push_reg,            "push the content of a register onto the stack.") \
    FOR_EACH(0xf,  NR_SKIP_NEQ,  0b011111, instr_skip_not_equal,      "if register p1 and register p2 not equal skip t bytes") \
    FOR_EACH(0x10, NR_SKIP_LEQ,  0b011111, instr_skip_less_equal,     "if register p1 and register p2 less or equal skip t bytes") \
    FOR_EACH(0x11, NR_SKIP_GEQ,  0b011111, instr_skip_greater_equal,  "if register p1 and register p2 greater or equal skip t bytes") \
    FOR_EACH(0x12, NR_MUL,       0b111111, instr_reg_mul,             "mul two registers into a target register (64 bit)") \
    FOR_EACH(0x13, NR_DIV,       0b111111, instr_reg_div,             "divide two registers into a target register (64 bit)") \
/*                                 ++++++      
 *                                 |||||+-> target
 *                                 ||||+--> param2
 *                                 |||+---> param1
 *                                 ||+----> param2 is register
 *                                 |+-----> param1 is register
 *                                 +------> target is register
 *
 * target type is always a register
 */

#define INSTR_JMP_TABLE(p1, p2, p3, label, p5, ...) &&label,
#define INSTR_ENUM_DEFINE(p1, p2, ...) p2 = p1,

typedef enum __neart_instr_codes_t {
    NEART_INSTR_FORECH(INSTR_ENUM_DEFINE)
} neart_instr_codes_t;

#define INSTR_SIZE_DEFINE(p1, p2, p3, ...) p2##_SIZE = \
    (\
        ((p3 & 0x1) ? 1 : 0) + \
        ((p3 & 0x2) ? ((p3 & 0x8) ? 1 : 4 ) : 0) + \
        ((p3 & 0x4) ? ((p3 & 0x10) ? 1 : 4 ) : 0) + \
        1 \
    ),
typedef enum __neart_instr_size_t {
    NEART_INSTR_FORECH(INSTR_SIZE_DEFINE)
} neart_instr_size_t;

#define INSTR_REG_COUNT_DEFINE(p1, p2, p3, ...) p2##_REG_COUNT = \
    (\
        ((p3 & 0x1) ? 1 : 0) + \
        ((p3 & 0x8) ? 1 : 0 ) + \
        ((p3 & 0x10) ? 1 : 0 ) \
    ),
typedef enum __neart_instr_register_count_t {
    NEART_INSTR_FORECH(INSTR_REG_COUNT_DEFINE)
} neart_instr_register_count_t;

//const char * ncode_names[255];

typedef int64_t register_t;
typedef int32_t stack_cell_t;
typedef int vreg_t;

struct __vmctx_t;

int neart_exec(struct __vmctx_t * code);

#endif /* _VM_H_ */
