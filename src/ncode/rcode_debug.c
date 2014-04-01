
#include "qcode.h"

static char _debug_param_type(int32_t type) {
    
    switch (type) {
        case PT_CONSTANT: return 'c';
        case PT_CPOOL_IDX: return 'p';
        case PT_REG: return 'r';
        default: return '_';
    }
}

void neart_debug_print_rcode(qcode_t * code) {

    int i = 0;
    char type = '\0';
    qinstr_t * instr = code->instr;
    while (i < code->instr_cursor) {

        printf("op(%d)\t", instr->instruction);
        type = _debug_param_type(instr->param1_type);
        printf("param1: ");
        printf("%c", type);
        if (type != '_') {
            printf("%d", instr->param1);
        }
        printf("\tparam2: ");
        type = _debug_param_type(instr->param2_type);
        printf("%c", type);
        if (type != '_') {
            printf("%d", instr->param2);
        }
        printf("\ttarg: ");
        printf("r%d\n", instr->target);

        instr++;
        i++;
    }
}
