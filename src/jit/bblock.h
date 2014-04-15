#ifndef BBLOCK_H
#define BBLOCK_H

#include "vm.h"
#include "rcode.h"
#include "klist.h"

struct __bblock;

#define __bb_free(x)
KLIST_INIT(bb, struct __bblock *, __bb_free)

// note that the basic bloc will not be garbage collected
typedef struct __bblock {

    /**
     * Access the register code instruction
     */
    rcode_t * instr;

    /**
     * How many instructions are included in this basic block?
     */
    int instr_count;

    /**
     * BB edges going out. initially NULL and will not be allocated unless it is needed.
     */
    klist_t(bb) * oedges;

    /**
     * The same as oedges but ingoing edges...
     */
    klist_t(bb) * iedges;

} bblock_t;

// a bbline is a collection of basic blocks one after another.
typedef struct __bbline {
    bblock_t * first;
    int cursor;
    int size;
} bbline_t;

bbline_t * n_bbline_alloc(void);

bblock_t * n_bblock_new(bbline_t * line);

void n_bbline_free(bbline_t*);

/**
 * Transform the function into a basic block format (bbline_t = { bbblock_t, bbblock_t, ... }).
 * Currently each basic block contains at MOST one instruction.
 */
bbline_t * n_bbnize(rcode_t * code);

#endif

