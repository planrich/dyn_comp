
#include "qcode.h"
#include "qcode_debug.h"

#include "vm.h"

#define INSTR_NAME(p1, p2, ...) if (p1 == code) { return #p2; }
static const char * _instr_name(rcode_t code) {
    NEART_INSTR_FORECH(INSTR_NAME)
    return NULL;
}
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

    int i = 0, c = 0;
    qinstr_t * instr = code->instr;
    while (i < code->instr_cursor) {

        if (instr->instruction == N_ENTER) {
            char * name = (char*)(neart_cpool_lookup(pool, instr->param1) + 4);
            printf("%s:\n", name);
            c = 0;
        }

        printf("%d:  ", c++);

        neart_debug_print_instr(instr);

        instr++;
        i++;
    }
}

void neart_debug_print_instr(qinstr_t * instr) {

    char type = '\0';
    printf("op(%10s|0x%x)  ", _instr_name(instr->instruction), instr->instruction);
    type = _debug_param_type(instr->param1_type);
    if (type != '_') {
        printf("P1: ");
        printf("%c", type);
        printf("%d", instr->param1);
    }
    type = _debug_param_type(instr->param2_type);
    if (type != '_') {
        printf("\tP2: ");
        printf("%c", type);
        printf("%d", instr->param2);
    }
    if (instr->target >= 0) {
        printf("\tT: ");
        printf("%d", instr->target);
    }
    printf("\n");
}

