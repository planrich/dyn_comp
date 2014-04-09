

#include "qcode.h"

#include "logging.h"
#include "config.h"
#include "utils.h"
#include "symt.h"
#include "gpir.h"
#include "vm.h"
#include "ast.h"
#include "cpool_builder.h"
#include "qcode_debug.h"

static const char hash_str_buffer[11];

static qinstr_t _instr(rcode_t i, int32_t p1, int32_t p1t, int32_t p2, int32_t p2t, int32_t t) {
    qinstr_t instr = { 
        .instruction = i, 
        .target = t, 
        .param1 = p1, 
        .param1_type = p1t,
        .param2 = p2, 
        .param2_type = p2t 
    };

    return instr;
}


typedef struct __ncode_gen_t {
    cpool_builder_t * cpool;
    qcode_t * code;

    uint8_t arguments[255];
    uint8_t registers[255];
    uint32_t reg;
    khash_t(str_int) * register_assoc;
} _ncode_gen_t;

static qcode_t * _qcode_alloc(void);
static int _qcode_append(qcode_t * code, qinstr_t instr);

static void _instr_move(qcode_t * code, sem_expr_t * se, int reg) {

    if (se->type == type_int) {
        int32_t p1 = neart_expr_to_int32(se->expr);
        qinstr_t instr = _instr(NR_L32, p1, PT_CONSTANT, 0, UNUSED, reg);
        _qcode_append(code, instr);
    } else if (se->type == type_generic) {
        // a variable
        int32_t p1 = se->assigned_register;
        if (p1 == -1 && (se->symbol_type & SYM_ARG) != 0) {
            // if it is an argument the variable references a parameter
            // thus it is already in the register 0-5 or on the stack
            p1 = se->argument_index;
            qinstr_t instr = _instr(NR_MOV, p1, PT_REG, 0, UNUSED, reg);
            _qcode_append(code, instr);
        } else {
            qinstr_t instr = _instr(NR_MOV, p1, PT_REG, 0, UNUSED, reg);
            _qcode_append(code, instr);
        }
    } else if (se->type == type_func) {
        int32_t p1 = se->assigned_register;
        qinstr_t instr = _instr(NR_MOV, p1, PT_REG, 0, UNUSED, reg);
        _qcode_append(code, instr);
    } else {
        IMPL_ME();
    }

    se->assigned_register = reg;
}

static void _gen_start_func(_ncode_gen_t * gen);
static int _generate_func(_ncode_gen_t * gen, func_t * func);
static int _generate_pattern(_ncode_gen_t * gen, func_t * func, pattern_t * pattern, int idx);
static void _instr_call(_ncode_gen_t * gen, sem_expr_t * spe);

static void _instr_load(_ncode_gen_t * gen, sem_expr_t * spe, klist_t(32) * stack);

qcode_t * neart_generate_register_code(module_t * module, cpool_builder_t * builder) {

    qcode_t * code = _qcode_alloc();

    ALLOC_STRUCT(_ncode_gen_t, gen);
    gen->reg = 0;
    gen->cpool = builder;
    gen->code = code;
    gen->register_assoc = kh_init(str_int);

    khiter_t k;
    khash_t(str_sym_entry_t) * h = module->symbols->symbols;

    for (k = kh_begin(h); k != kh_end(h); ++k) {
        if (kh_exist(h, k)) {
            sym_entry_t * entry = &kh_val(h, k);
            if (sym_entry_is(entry, SYM_FUNC)) {
                //NEART_LOG_DEBUG("generating function %s\n", entry->func->name);
                _generate_func(gen, entry->func);
            }
        }
    }

#ifdef NEART_DEBUG
    neart_debug_print_rcode(code, builder->sym_pool);
#endif

    kh_destroy(str_int, gen->register_assoc);

    return code;
}


static int _generate_func(_ncode_gen_t * gen, func_t * func) {

    klist_t(pattern_t) * l = func->patterns;
    params_t * params = func->params;

    _gen_start_func(gen);

    uint32_t idx = neart_cpool_builder_find_or_reserve_index(gen->cpool, func->name);
    qinstr_t instr = _instr(N_ENTER, idx, PT_CPOOL_IDX, 0, UNUSED, UNUSED);
    _qcode_append(gen->code, instr);

    kliter_t(pattern_t) * k;
    int i = 0;
    for (k = kl_begin(l); k != kl_end(l); k = kl_next(k)) {
        pattern_t * pattern = kl_val(k);
        _generate_pattern(gen, func, pattern, i++);
    }

    return 0;
}


static int _generate_pattern(_ncode_gen_t * gen, func_t * func, pattern_t * pattern, int idx) {

    sem_expr_t * se = pattern->expr;

    if (se->apply) {
        // a function call
        _instr_call(gen, se);
    } else {
        // this is just a constant value
        _instr_move(gen->code, pattern->expr, 6);
    }

    // it is not in the return register? move it!
    if (se->assigned_register != 6) {
        qinstr_t instr = _instr(NR_MOV, se->assigned_register, PT_REG, 0, UNUSED, 6);
        _qcode_append(gen->code, instr);
    }

    qinstr_t instr = _instr(N_END, 0, UNUSED, 0, UNUSED, UNUSED);
    _qcode_append(gen->code, instr);

    return 1;
}

int n_ra_reg(_ncode_gen_t * gen) {
    // push gen->reg content to stack

    return gen->reg++;
}

static void _instr_call_param(_ncode_gen_t * gen, sem_expr_t * se, int index) {

    if (se->apply) {
        // a function call
        _instr_call(gen, se);
    } else {
        _instr_move(gen->code, se, index);
    }

}

static void _instr_call(_ncode_gen_t * gen, sem_expr_t * se) {

    qinstr_t instr;
    int param_count = neart_params_count(se->func->params);

    sem_expr_t * cursor = se->next;
    int reg = 0;
    while (cursor != NULL) {
        // CHECK if arg must be saved
        if (reg < param_count) {
            //_instr_(reg);
            // save this register somewhere
            //cursor->assigned_register = reg++;
        }
        cursor = cursor->next;
    }

    if (se->type == type_func_builtin) {
        if (se->expr->type == ET_OP_IADD) {
            int t = n_ra_reg(gen);
            se->assigned_register = t;

            int p1 = n_ra_reg(gen);
            int p2 = n_ra_reg(gen);
            _instr_call_param(gen, se->next, p1);
            se->next->assigned_register = p1;
            _instr_call_param(gen, se->next->next, p2);
            se->next->assigned_register = p2;

            instr = _instr(NR_ADD, p1, PT_REG, p2, PT_REG, t);
            _qcode_append(gen->code, instr);
        } else {
            IMPL_ME();
        }
    } else if (se->type == type_func) {
        int i = 0;

        if (param_count > 5) {
            NEART_LOG_FATAL("cannot handle more than 5 params ... yet\n");
        }

        sem_expr_t * cur = se->next;
        for (i = 0; i < param_count; i++) {
            printf("param %d of type %c %s\n", i, cur->type, cur->expr->data);
            _instr_call_param(gen, cur, i);
            if (cur->assigned_register != i) {
                _instr_move(gen->code, cur, i);
            }
            cur = cur->next;
        }

        uint32_t idx = neart_cpool_builder_find_or_reserve_index(gen->cpool, se->func->name);
        instr = _instr(N_CALL, idx, PT_CPOOL_IDX, 0, UNUSED, UNUSED);
        _qcode_append(gen->code, instr);
        se->assigned_register = 6;

    } else {
        IMPL_ME();
    }

}

static int _qcode_append(qcode_t * code, qinstr_t instr) {

    if (code->instr_cursor >= code->instr_count) {
        code->instr_count *= 2;
        code->instr = realloc(code->instr, sizeof(qinstr_t) * (code->instr_count));
    }

    qinstr_t * instr_p = code->instr + code->instr_cursor;
    *instr_p = instr;
    code->instr_cursor++;

    return 0;
}

static void _gen_start_func(_ncode_gen_t * gen) {
    gen->reg = 6;
}


static qcode_t * _qcode_alloc(void) {
    ALLOC_STRUCT(qcode_t, code);
    code->instr_count = 16;
    code->instr_cursor = 0;
    code->instr = malloc(sizeof(qinstr_t) * 16);
    return code;
}


static void _instr_load(_ncode_gen_t * gen, sem_expr_t * spe, klist_t(32) * stack) {

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

