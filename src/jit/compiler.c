#include "compiler.h"

#include "bblock.h"
#include "vm.h"
#include "logging.h"
#include "reg_alloc.h"
#include "x86_64.h"
#include "gc.h"

#define MCODE_SIZE 64

static mcode_t * _jit_methods = NULL;

mcode_t * neart_jit_template_transform(bbline_t * line, life_range_t * ranges);

mcode_t * neart_jit_compile(vmctx_t * vmc, rcode_t * code) {

    void * machine_code = NULL;

    if (_jit_methods == NULL) {
        NEART_LOG_DEBUG("alloc jit methods table of size %d\n", vmc->symbols->size);
        _jit_methods = GC_MALLOC(sizeof(void*) * vmc->symbols->size);
    }

    bbline_t * line = neart_bbnize(code);

    // calculate life ranges
    life_range_t * life_ranges = neart_ra_life_ranges(line);

    // then invoke the assembler
    return neart_jit_template_transform(line, life_ranges);
}


mcode_t * neart_jit_template_transform(bbline_t * line, life_range_t * ranges) {

    mcode_t * mcode = GC_MALLOC(sizeof(MCODE_SIZE));
    int mcode_size = MCODE_SIZE;

    for (int i = 0; i < line->size; i++) {
        bblock_t * block = line->first + i;
    }


    return mcode;
}

/*
mcode_t * _check_and_resize(mcode_t * mcode, int * mcode_size, int new_size) {
    if (mcode_size < new_size) {
        mcode = GC_REALLOC(mcode, new_size);
        *mcode_size = new_size;
    }
    return mcode;
}*/
