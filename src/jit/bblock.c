#include "bblock.h"

#include <stdlib.h>
#include "utils.h"
#include "logging.h"

#define INIT_SIZE 64

bblock_t * _rcode_find(bbline_t * line, bblock_t * start, int i, int32_t byte_offset);

bbline_t * n_bbline_alloc(void) {

    ALLOC_STRUCT(bbline_t, line);
    line->first = malloc(sizeof(bblock_t) * INIT_SIZE);
    line->cursor = 0;
    line->size = INIT_SIZE;

    return line;
}

bbline_t * n_bbline_align(bbline_t * line) {
    //line->first = realloc(line->first, sizeof(bblock_t) * line->cursor);
    line->size = line->cursor;
    return line;
}

bblock_t * n_bblock_new(bbline_t * line) {

    if (line->cursor >= line->size) {
        line->size *= 2;
        line->first = realloc(line->first, sizeof(bblock_t) * line->size);
    }

    bblock_t * bb = line->first + line->cursor;
    line->cursor++;

    bb->instr_count = 1;
    bb->oedges = NULL;
    bb->iedges = NULL;

    return bb;
}

void n_bbline_free(bbline_t * line) {

    for (int i = 0; i < line->size; i++) {
        bblock_t * block = line->first + i;
        if (block->oedges != NULL) {
            kl_destroy(bb, block->oedges);
        }
        if (block->iedges != NULL) {
            kl_destroy(bb, block->iedges);
        }
    }

    free(line->first);
    free(line);
}

#define _SWITCH_INSTR_BBNEW(p1, p2, p3, ...) \
    case p2: {\
        block->instr = wptr; \
        skip_bytes = p2##_SIZE;\
        break; \
    }

bbline_t * neart_bbnize(rcode_t * code) {

    rcode_t * wptr = code;
    rcode_t * dptr = NULL;
    int skip_bytes = 0;

    if (*wptr != N_ENTER) {
        NEART_LOG_FATAL("beginning is NOT ENTER\n");
        return NULL;
    }
    //wptr += N_ENTER_SIZE;
    bbline_t * line = n_bbline_alloc();

    while (1) {

        bblock_t * block = n_bblock_new(line);
        switch (*wptr) {
            // creates some thing like
            // 
            //  case INSTR: \
            //      block->instr = wptr; \
            //      skip_bytes = INSTR_SIZE; \
            //      break;
            //
            //  for each instruction
            //
            NEART_INSTR_FORECH(_SWITCH_INSTR_BBNEW)

            default:
                NEART_LOG_FATAL("did not implement op code %x\n", *wptr);
        }

        if (*wptr == N_METH_BOUND) {
            break;
        }

        wptr += skip_bytes;
    }

    n_bbline_align(line);

    NEART_LOG_DEBUG("created %d basic blocks for this func\n", line->size);

    // now that all basic blocks have be created -> create the control
    // flow edges...
    for (int i = 0; i < line->size; i++) {
        bblock_t * block = line->first + i;
        int byte_offset = n_rcode_jmp_offset(block->instr);
        if (byte_offset == 0) { // not a jmp instr
            continue;
        }

        bblock_t * target = _rcode_find(line, block, i, byte_offset);
        if (target == NULL) {
            NEART_LOG_FATAL("could not find basic block which has %d bytes offset\n", byte_offset);
            n_bbline_free(line);
            return NULL;
        }

        // connect the edges
        if (block->oedges == NULL) {
            block->oedges = kl_init(bb);
        }
        *kl_pushp(bb, block->oedges) = target;

        if (target->iedges == NULL) {
            target->iedges = kl_init(bb);
        }
        *kl_pushp(bb, target->iedges) = block;
    }

    return line;
}

bblock_t * _rcode_find(bbline_t * line, bblock_t * start, int i, int32_t byte_offset) {

    rcode_t * base = start->instr;
    int32_t diff;

    while (byte_offset < 0) {

        if (i > 1) {
            i--;
        } else {
            // cannot search beyond zero
            return NULL;
        }

        bblock_t * target = line->first + i;
        diff = abs(base - target->instr);
        if (diff == byte_offset) {
            // we found it!
            return target;
        } else if (diff > byte_offset) {
            // if the diff is bigger than the offset the
            // basic block could not be found
            return NULL;
        }
    }

    base = (start+1)->instr;

    while (byte_offset > 0) {
        if (i < line->size) {
            i++;
        } else {
            // cannot search beyond max
            return NULL;
        }

        bblock_t * target = line->first + i;
        diff = abs(base - target->instr);
        //printf("instr %d %d\n", *target->instr, diff);
        if (diff == byte_offset) {
            // we found it!
            return target;
        }
    }

    return NULL;
}
