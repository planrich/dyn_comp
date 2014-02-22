

#include "qcode.h"

#include "logging.h"
#include "utils.h"
#include "symt.h"
#include "gpir.h"
#include "vm.h"
#include "ast.h"
#include "cpool_builder.h"

#define PT_REG (0)
#define PT_CPOOL_IDX (1)

#define UNUSED (-1)

static const char hash_str_buffer[11];

static const qinstr_t ret_instr = { .instruction = N_END, .target = 0, .param1 = 0, .param1_type = 0, 
 .param2 = 0, .param2_type = 0 };


static void _debug_print_qcode(qcode_t * code) {

    int i = 0;
    qinstr_t * instr = code->instr;
    while (i < code->instr_cursor) {

        printf("op(%d) p1: r%d p2: r%d t: r%d\n", instr->instruction, instr->param1, instr->param2, instr->target);

        instr++;
        i++;
    }

}

typedef struct __ncode_gen_t {
    cpool_builder_t * cpool;
    qcode_t * code;

    uint8_t arguments[255];
    uint8_t registers[255];
    uint32_t reg;
    khash_t(str_int) * register_assoc;
} _ncode_gen_t;

static qcode_t * _qcode_alloc(void) {
    ALLOC_STRUCT(qcode_t, code);
    code->instr_count = 16;
    code->instr_cursor = 0;
    code->instr = malloc(sizeof(qinstr_t) * 16);
    return code;
}

static int _qcode_append(qcode_t * code, qinstr_t instr) {

    if (code->instr_cursor >= code->instr_count) {
        code->instr = realloc(code->instr, code->instr_count * 2);
        code->instr_count *= 2;
    }

    qinstr_t * instr_p = code->instr + code->instr_cursor;
    *instr_p = instr;
    code->instr_cursor++;

    return 0;
}

static int _generate_func(_ncode_gen_t * gen, func_t * func);
static int _generate_pattern(_ncode_gen_t * gen, func_t * func, pattern_t * pattern, int idx);

static void _instr_apply(_ncode_gen_t * gen, sem_post_expr_t * spe, klist_t(32) * stack) {

    qinstr_t instr;

    if (spe->type == type_func_builtin) {
        if (spe->expr->type == ET_OP_IADD) {
            instr.instruction = NR_ADD;
            instr.param1_type = PT_REG;
            kl_shift(32, stack, &instr.param1);
            instr.param2_type = PT_REG;
            kl_shift(32, stack, &instr.param2);
            instr.target = gen->reg++;
            *kl_pushp(32, stack) = instr.target;
            _qcode_append(gen->code, instr);
        }
    } else if (spe->type == type_func) {
        int i = 0;

        instr.target = 0;

        if (neart_params_count(spe->func->params) > 5) {
            NEART_LOG_FATAL("cannot handle more than 5 params ... yet\n");
        }
        // this is naive! what if the register is dirty?
        for (i = 0; i < neart_params_count(spe->func->params); i++) {
            instr.instruction = NR_L32;
            instr.param1_type = PT_REG;
            instr.param1 = i;
            kl_shift(32, stack, &instr.param2);

            _qcode_append(gen->code, instr);
        }

        instr.instruction = NR_CALL;
        instr.param1_type = PT_CPOOL_IDX;
        uint32_t idx = neart_cpool_builder_find_or_reserve_index(gen->cpool, spe->func->name);
        instr.param1 = (int32_t)idx;
        instr.param2 = 0;
        instr.param2_type = 0;
        instr.target = 0;

        _qcode_append(gen->code, instr);

    } else {
        NEART_LOG_FATAL("incomplete impl of instr apply %c\n", spe->type);
    }
}
static void _instr_load(_ncode_gen_t * gen, sem_post_expr_t * spe, klist_t(32) * stack) {

    qinstr_t instr;
    int ret;
    khiter_t k;

    if ((spe->symbol_type & SYM_ARG) != 0) {

        if (spe->type == type_int) {
            *kl_pushp(32, stack) = neart_expr_to_int32(spe->expr);
        } else if (spe->type == type_generic && spe->argument_index <= 5) {
            // args are in specific registers (from 0-5). 6+ are on the stack
            *kl_pushp(32, stack) = spe->argument_index;
        } else if (spe->argument_index > 5) {
            NEART_LOG_FATAL("cannot handle more than 5 arguments. yet...\n");
        } else {
            NEART_LOG_FATAL("cannot handle load argument\n");
        }
    }
}

static int _generate_pattern(_ncode_gen_t * gen, func_t * func, pattern_t * pattern, int idx) {

    klist_t(32) * stack = kl_init(32);

    sem_post_expr_t * cursor = pattern->expr;

    sym_table_t * symt = pattern->symbols;

    while (cursor != NULL) {

        if (!cursor->apply) {
            _instr_load(gen, cursor, stack);
        } else {
            _instr_apply(gen, cursor, stack);
        }

        cursor = cursor->next;
    }

    kl_destroy(32, stack);

    return 1;
}

qcode_t * neart_generate_register_code(module_t * module) {

    NEART_LOG(LOG_INFO, "invoking register code generation\n");

    qcode_t * code = _qcode_alloc();

    ALLOC_STRUCT(cpool_builder_t, cpool);

    ALLOC_STRUCT(_ncode_gen_t, gen);
    gen->reg = 0;
    gen->cpool = neart_cpool_builder_alloc();
    gen->code = code;
    gen->register_assoc = kh_init(str_int);

    khiter_t k;
    khash_t(str_sym_entry_t) * h = module->symbols->symbols;

    for (k = kh_begin(h); k != kh_end(h); ++k) {
        if (kh_exist(h, k)) {
            sym_entry_t * entry = &kh_val(h, k);
            if (sym_entry_is(entry, SYM_FUNC)) {
                NEART_LOG_DEBUG("generating function %s\n", entry->func->name);
                _generate_func(gen, entry->func);
            }
        }
    }

    _debug_print_qcode(code);

    kh_destroy(str_int, gen->register_assoc);

    return NULL;
}

static void _gen_start_func(_ncode_gen_t * gen) {

    printf("starting new func context\n");
    gen->reg = 6;
}


static int _generate_func(_ncode_gen_t * gen, func_t * func) {

    printf("generating %s\n", func->name);
    klist_t(pattern_t) * l = func->patterns;
    params_t * params = func->params;

    _gen_start_func(gen);

    neart_cpool_builder_find_or_reserve_index(gen->cpool, func->name);

    kliter_t(pattern_t) * k;
    int i = 0;
    for (k = kl_begin(l); k != kl_end(l); k = kl_next(k)) {
        pattern_t * pattern = kl_val(k);
        _generate_pattern(gen, func, pattern, i++);
    }

    _qcode_append(gen->code, ret_instr);

    return 0;
}
