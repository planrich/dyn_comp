
#include "qcode.h"
#include "qcode_debug.h"

#include "vm.h"

static char _debug_param_type(int32_t type) {
    
    switch (type) {
        case PT_CONSTANT: return 'c';
        case PT_CPOOL_IDX: return 'p';
        case PT_REG: return 'r';
        case UNUSED: return '_';
        default: return 'w';
    }
}

void neart_debug_print_rcode(qcode_t * code, cpool_t * pool) {

    int i = 0;
    qinstr_t * instr = code->instr;
    while (i < code->instr_cursor) {

        if (instr->instruction == N_ENTER) {
            char * name = (char*)(neart_cpool_lookup(pool, instr->param1) + 4);
            printf("%s:\n", name);
        }

        printf("  ");

        neart_debug_print_instr(instr);

        instr++;
        i++;
    }
}

void neart_debug_print_instr(qinstr_t * instr) {

    char type = '\0';
    printf("op(%d)\t", instr->instruction);
    type = _debug_param_type(instr->param1_type);
    printf("P1: ");
    printf("%c", type);
    if (type != '_') {
        printf("%d", instr->param1);
    }
    printf("\tP2: ");
    type = _debug_param_type(instr->param2_type);
    printf("%c", type);
    if (type != '_') {
        printf("%d", instr->param2);
    }
    printf("\tT: ");
    if (instr->target >= 0) {
        printf("r%d\n", instr->target);
    } else {
        printf("_\n");
    }
}

