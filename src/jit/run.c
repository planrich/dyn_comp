#include "run.h"

#include "compiler.h"
#include "logging.h"

typedef int (*func_ptr)();

int neart_jit_exec(vmctx_t * ctx) {

    rcode_t * main = ctx->code + ctx->main_offset;

    func_ptr f = (func_ptr)neart_jit_compile(ctx, main);

    int result = f();
    return result;
}
