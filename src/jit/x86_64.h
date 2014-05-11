
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

#define HW_GP_REG_COUNT (14)

typedef struct __ra_state_t {
    ra_t registers[HW_GP_REG_COUNT];
} ra_state_t;

typedef int32_t reg_state_t;
typedef int32_t hwreg_t;

void arch_call(memio_t * io, void * func, void * arg1, void * arg2);
void arch_load_32(memio_t * io, int32_t c, hwreg_t vreg);
void arch_ret(memio_t * io);

hwreg_t arch_ra_hwreg(ra_state_t * state, life_range_t * ranges, vreg_t reg, int time_step);
/**
 * Is a hardware register in use?
 */
int arch_ra_hwreg_in_use(ra_state_t * state, hwreg_t reg);

ra_state_t * arch_ra_state_new(void);


#endif
