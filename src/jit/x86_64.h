
#ifndef X86_64_H
#define X86_64_H

#include "compiler.h"
#include "memio.h"
#include "reg_alloc.h"
#include "collections.h"
#include "vm.h"

// 16 x86 64 general purpose registers
typedef enum __qw_registers {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4, // the stack pointer
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
} qw_registers_t;

typedef struct __ra_t {
    vreg_t v_reg;
    qw_registers_t hw_reg;
    life_range_t * range;
} ra_t;

#define HW_REG_COUNT (16)
#define HW_GP_REG_COUNT (7)
#define HW_ROUTINE_REG_COUNT (7)

typedef struct __ra_state_t {
    life_range_t * ranges;
    int time_step;
    ra_t registers[HW_REG_COUNT];
    // this displacement is used for GP registers (first 7) and then for routine registers (the latter7)
    char reg_displacement[HW_GP_REG_COUNT + HW_ROUTINE_REG_COUNT];
} ra_state_t;

typedef int32_t reg_state_t;
typedef int32_t hwreg_t;

void arch_call(memio_t * io, ra_state_t * state, void * func, void * arg1, void * arg2, int time_step);
void arch_load_32(memio_t * io, int32_t c, hwreg_t vreg);
void arch_push_const(memio_t * io, int32_t c);

/**
 * Return from a routine call.
 */
void arch_ret(memio_t * io, int var_byte_count);

/**
 * Get the hardware register for this virtual register.
 */
hwreg_t arch_ra_hwreg(ra_state_t * state, vreg_t reg);

/**
 * Save the hardware register on the stack
 */
int arch_save_hw_reg(memio_t * io, ra_state_t * state, hwreg_t reg);

/**
 * Restore the hardware register from the stack.
 */
void arch_restore_hw_reg(memio_t * io, hwreg_t reg);

/**
 * Is a hardware register in use?
 */
int arch_ra_hwreg_in_use(ra_state_t * state, hwreg_t reg);

/**
 * Move 64 bit to a register. (movabsq)
 */
void arch_move_long(memio_t * io, hwreg_t reg, void * ptr);

ra_state_t * arch_ra_state_new(void);

/**
 * Enter a routine. arch_ret is the inverse.
 */
void arch_enter_routine(memio_t * io, int var_count_bytes);

/**
 * move a register to another register.
 */
void arch_move_reg(memio_t * io, ra_state_t * state, vreg_t source, vreg_t target);

/**
 * add two registers into a third one
 */
void arch_add_reg(memio_t * io, ra_state_t * state, vreg_t s1, vreg_t s2, vreg_t target);

#endif
