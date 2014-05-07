#include "reg_alloc.h"
#include "utils.h"
#include "logging.h"
#include "gc.h"
#include "vm.h"
#include "x86_64.h"

#define REG_COUNT(p1, p2, ...) p2##_REG_COUNT,
static int _reg_count[] = {
    NEART_INSTR_FORECH(REG_COUNT)
};

int _ra_used_reg(life_range_t * ranges, int max_register, int reg, int basic_block) {

    if (reg > max_register) {
        ranges = GC_REALLOC(ranges, sizeof(life_range_t) * reg);
        max_register = reg;
    }

    life_range_t * range = ranges + reg;
    if (range->reg != reg) {
        range->reg = reg;
        range->start = basic_block;
        range->end = basic_block;
    } else {
        range->end = basic_block;
    }

    return max_register;
}

life_range_t * neart_ra_life_ranges(bbline_t * line) {

    int max_register = 5;
    life_range_t * ranges = GC_MALLOC(sizeof(life_range_t)*max_register);
    ranges->reg = -1; // the first reg must not equal 0 -> otherwise it is
                      // recognized as 'valid' life range

    for (int i = 0; i < line->size; i++) {
        bblock_t * block = line->first + i;
        rcode_t * instr = block->instr;

        int offset = 0;
        int param = neart_parameter_usage(*instr);

        // when calling all parameter registers have the live range until
        // that point
        if (*instr == N_CALL) {
            for (int j = 0; j < 6; j++) {
                life_range_t * range = ranges + j;
                if (range->reg == j) {
                    range->end = i;
                }
            }
        }

        instr += 1;
        if (INSTR_IS_REG_PARAM1(param)) {
            max_register = _ra_used_reg(ranges, max_register, *(instr + offset), i);
            offset += 1;
        } else if (INSTR_USES_PARAM1(param)) {
            offset += 4;
        }

        if (INSTR_IS_REG_PARAM2(param)) {
            max_register = _ra_used_reg(ranges, max_register, *(instr + offset), i);
            offset += 1;
        } else if (INSTR_USES_PARAM2(param)) {
            offset += 4;
        }

        if (INSTR_IS_REG_TARGET(param)) {
            max_register = _ra_used_reg(ranges, max_register, *(instr + offset), i);
        }
    }

    for (int i = 0; i < max_register; i++) {
        life_range_t * range = ranges + i;
        if (range->reg == i) {
            NEART_LOG_DEBUG("%d range (%d-%d)\n", range->reg, range->start, range->end);
        }
    }

    return ranges;
}
