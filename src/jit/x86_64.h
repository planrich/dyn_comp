
#ifndef X86_64_H
#define X86_64_H

#include "compiler.h"
#include "memio.h"
#include "reg_alloc.h"

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

typedef struct __ra_state_t {

    klist_t(32) * klist;


} ra_state_t;

typedef int32_t reg_state_t;
typedef uint32_t hwreg_t;
typedef int vreg_t;

void arch_load_32(memio_t * io, int32_t c, hwreg_t vreg);

void arch_ret(memio_t * io);

hwreg_t arch_ra_hwreg(ra_state_t * state, life_range_t * ranges, vreg_t reg);

#endif
