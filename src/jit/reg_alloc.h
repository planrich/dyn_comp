#ifndef REGISTER_ALLOCATOR_H
#define REGISTER_ALLOCATOR_H

#include "collections.h"
#include "vm.h"
#include "inttypes.h"
#include "bblock.h"
#include "x86_64.h"

// bbline is considered not to have more than 2^16 basic blocks
typedef struct __life_range {
    uint8_t reg;
    uint16_t start;
    uint16_t end;
} life_range_t;

hwreg_t neart_ra_use(reg_state_t * reg_state, life_range_t * ranges, int reg, int block);

void neart_ra_release(reg_state_t * reg_state, hwreg_t reg);

life_range_t * neart_ra_life_ranges(bbline_t * line);

#endif
