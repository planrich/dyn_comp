#ifndef LOADER_H
#define LOADER_H

#include "rcode.h"
#include "vm.h"

typedef struct __vmctx_t {
    rcode_t * code;
    cpool_t * symbols;
    register_t registers[128];
    stack_cell_t * stack;
    uint32_t main_offset;
} vmctx_t;

vmctx_t * neart_vmctx_alloc(void);
void neart_vmctx_free(vmctx_t * ctx);

vmctx_t * neart_load_rcode_file(const char * name);

#endif
