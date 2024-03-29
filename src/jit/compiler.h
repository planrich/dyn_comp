#ifndef COMPILER_H
#define COMPILER_H

#include "rcode.h"
#include "loader.h"

typedef char mcode_t;

/**
 * compile register code to native machine code.
 * This implementation is x86 64 bit specific.
 */
mcode_t * neart_jit_compile(vmctx_t * vmc, rcode_t * code);

void jit_print_time(void);

#endif
