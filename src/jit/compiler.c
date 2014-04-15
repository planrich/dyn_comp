#include "compiler.h"

#include "bblock.h"
#include "vm.h"
#include "logging.h"

#include "x86_64.h"

mcode_t * n_jit_compile(vmctx_t * vmc, rcode_t * code) {

    void * machine_code = NULL;

    bbline_t * line = n_bbnize(code);

    // calculate life ranges
    // life_ranges = n_ra_life_ranges(line);
    //
    // then invoke the assembler
    // machine_code = n_jist_template_transform(line, life_ranges);

    return machine_code;
}

void n_jit_template_transform(rcode_t * code, rcode_t * end, mcode_t * __mc) {
    /*
    rcode_t * wptr = code;
    rcode_t * dptr = NULL;
    register_t r1;
    register_t r2;
    register_t r3;

    register_t registers[255];
    n_rallocator(code, end, registers);

    while (wptr < end) {
        switch (*wptr) {
            case N_ENTER:
                wptr += N_ENTER_SIZE;
                break;
            case NR_L32:
                dptr = wptr + 2;

                arch_load_32(__mc, dptr);

                wptr += NR_L32_SIZE;
                break;

            default:
                NEART_LOG_FATAL("did not implement op code %x\n", *code);
        }
    }
    */
}
