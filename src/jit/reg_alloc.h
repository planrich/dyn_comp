#ifndef REGISTER_ALLOCATOR_H
#define REGISTER_ALLOCATOR_H

#include "collections.h"
#include "vm.h"
#include "inttypes.h"
#include "bblock.h"

// bbline is considered not to have more than 2^16 basic blocks
typedef struct __life_range {
    uint8_t reg;
    uint16_t start;
    uint16_t end;
} life_range_t;

life_range_t * neart_ra_life_ranges(bbline_t * line);

#endif
