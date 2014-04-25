
#include "rcode.h"
#include "vm.h"

int32_t n_rcode_jmp_offset(rcode_t * rcode) {

    rcode_t c = *rcode;

    if (c == NR_SKIP_EQ) {
        return *(rcode + 3); // skip op, p1, p2 -> return t
    } else if (c == NR_JMP) {
        return *(rcode + 1); // skip op -> return t
    }

    return 0;
}


#define INSTR_PARAM_TYPE(p1,p2,mask, ...) mask,
int neart_parameter_usage(rcode_t instr) {
    static char param_type[] = { NEART_INSTR_FORECH(INSTR_PARAM_TYPE) };

    return param_type[instr];
}
