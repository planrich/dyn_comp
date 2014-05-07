

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

typedef struct __ncode_gen_t {
    cpool_builder_t * cpool;
    qcode_t * code;

    uint32_t reg;
    khash_t(str_int) * register_assoc;
} _ncode_gen_t;

static const char hash_str_buffer[11];

static qcode_t * _qcode_alloc(void);
static int _qcode_append(qcode_t * code, qinstr_t instr);
static qcode_t * _instructions(_ncode_gen_t * gen, sem_expr_t * se, int target);
static void _instr_if(_ncode_gen_t * gen, sem_expr_t * se, int target);
static qinstr_t _instr(rcode_t i, int32_t p1, int32_t p1t, int32_t p2, int32_t p2t, int32_t t);
static void _instr_move(qcode_t * code, sem_expr_t * se, int reg);
static void _gen_start_func(_ncode_gen_t * gen);
static int _generate_func(_ncode_gen_t * gen, func_t * func);
static int _generate_pattern(_ncode_gen_t * gen, func_t * func, pattern_t * pattern, int idx);
static void _instr_call(_ncode_gen_t * gen, sem_expr_t * spe, int target);
static void _qcode_concat(qcode_t * code, qcode_t * toappend);
static qinstr_t * _qcode_last(qcode_t * code);
int n_ra_reg(_ncode_gen_t * gen);

qcode_t * neart_generate_register_code(module_t * module, cpool_builder_t * builder) {

    qcode_t * code = _qcode_alloc();

    GC_ALLOC_STRUCT(_ncode_gen_t, gen);
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

    instr = _instr(N_METH_BOUND, 0, UNUSED, 0, UNUSED, UNUSED);
    _qcode_append(gen->code, instr);

    return 0;
}

static int _generate_pattern(_ncode_gen_t * gen, func_t * func, pattern_t * pattern, int idx) {

    sem_expr_t * se = pattern->expr;

    qcode_t * code = _instructions(gen, se, 6);
    _qcode_concat(gen->code, code);

    qinstr_t instr = _instr(N_END, 0, UNUSED, 0, UNUSED, UNUSED);
    _qcode_append(gen->code, instr);

    return 1;
}

static void _instr_if(_ncode_gen_t * gen, sem_expr_t * se, int target) {

    qcode_t * cond = _instructions(gen, se->detail, -1);
    qcode_t * then = _instructions(gen, se->next->detail, target);
    qcode_t * _else = _instructions(gen, se->next->next->detail, target);

    if (target != UNUSED) {
        _qcode_append(then, _instr(N_END, 0, UNUSED, 0, UNUSED, UNUSED));
        _qcode_append(_else, _instr(N_END, 0, UNUSED, 0, UNUSED, UNUSED));
    }

    qinstr_t * last = _qcode_last(cond);
    int _else_bytes = n_ncode_bytes(_else);
    last->target = _else_bytes;

    _qcode_concat(gen->code, cond);
    _qcode_concat(gen->code, _else);
    _qcode_concat(gen->code, then);

    se->assigned_register = target;


}

static qcode_t * _instructions(_ncode_gen_t * gen, sem_expr_t * se, int target) {
    qcode_t * code = _qcode_alloc();

    _ncode_gen_t g = { .code = code, .reg = gen->reg, .cpool = gen->cpool };

    // when nesting is encountered the data is in the detail branch
    if (se->lang_construct == construct_nest) {
        se = se->detail;
    }

    if (se->lang_construct == construct_func && se->apply) {
        // a function call
        _instr_call(&g, se, target);
    } else if (se->lang_construct == construct_if) {
        _instr_if(&g, se, target);
    } else if (se->lang_construct == construct_param) {
        // this is just a constant value
        if (target == -1) { NEART_LOG_FATAL("expected target register to be already specified\n"); }
        _instr_move(code, se, target);
    } else {
        IMPL_ME();
    }

    gen->reg = g.reg;

    return code;
}

int n_ra_reg(_ncode_gen_t * gen) {
    // push gen->reg content to stack

    return gen->reg++;
}

static void _instr_call_param(_ncode_gen_t * gen, sem_expr_t * se, int index) {
    qcode_t * code = _instructions(gen, se, index);
    _qcode_concat(gen->code, code);
    se->assigned_register = index;
}

static void _instr_call(_ncode_gen_t * gen, sem_expr_t * se, int target) {

    qinstr_t instr;
    int param_count = neart_params_count(se->func->params);

    sem_expr_t * cursor = se->next;

    if (se->type == type_func_builtin) {
        if (se->expr->type == ET_OP_IADD) {
            int t = n_ra_reg(gen);
            se->assigned_register = t;

            int p1 = n_ra_reg(gen);
            int p2 = n_ra_reg(gen);
            _instr_call_param(gen, se->next, p1);
            _instr_call_param(gen, se->next->next, p2);

            instr = _instr(NR_ADD, p1, PT_REG, p2, PT_REG, t);
            _qcode_append(gen->code, instr);
        } else if (se->expr->type == ET_OP_EQUAL) {
            int p1 = n_ra_reg(gen);
            int p2 = n_ra_reg(gen);
            _instr_call_param(gen, se->next, p1);
            _instr_call_param(gen, se->next->next, p2);

            instr = _instr(NR_SKIP_EQ, p1, PT_REG, p2, PT_REG, 0);
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
            _instr_call_param(gen, cur, i);
            cur = cur->next;
        }

        uint32_t idx = neart_cpool_builder_find_or_reserve_index(gen->cpool, se->func->name);
        instr = _instr(N_CALL, idx, PT_CPOOL_IDX, 0, UNUSED, UNUSED);
        _qcode_append(gen->code, instr);
        se->assigned_register = 6;

    } else {
        IMPL_ME();
    }


    if (se->assigned_register != target) {
        qinstr_t instr = _instr(NR_MOV, se->assigned_register, PT_REG, 0, UNUSED, target);
        _qcode_append(gen->code, instr);
    }
}

static qinstr_t * _qcode_last(qcode_t * code) {
    return code->instr + code->instr_cursor - 1;
}

static void _qcode_concat(qcode_t * code, qcode_t * toappend) {

    qinstr_t * instr = toappend->instr;

    int i = 0;
    while (i < toappend->instr_cursor) {
        _qcode_append(code, *instr);

        instr++;
        i++;
    }
}

static int _qcode_append(qcode_t * code, qinstr_t instr) {

    if (code->instr_cursor >= code->instr_count) {
        code->instr_count *= 2;
        code->instr = GC_REALLOC(code->instr, sizeof(qinstr_t) * (code->instr_count));
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
    GC_ALLOC_STRUCT(qcode_t, code);
    code->instr_count = 16;
    code->instr_cursor = 0;
    code->instr = GC_MALLOC(sizeof(qinstr_t) * 16);
    return code;
}

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

uint32_t n_ncode_bytes(qcode_t * code) {
    uint32_t count = 0;
    int i = 0;
    qinstr_t * instr = code->instr;
    while (i < code->instr_cursor) {
        int32_t uses = neart_parameter_usage(instr->instruction);
        int32_t p1 = instr->param1;
        int32_t p2 = instr->param2;

        count += 1;

        // parameter number 1
        if (INSTR_USES_PARAM1(uses)) {
            count += (INSTR_IS_REG_PARAM1(uses) ? 1 : 4);
        }
        // parameter number 2
        if (INSTR_USES_PARAM2(uses)) {
            count += (INSTR_IS_REG_PARAM2(uses) ? 1 : 4);
        }
        // the target
        if (INSTR_USES_TARGET(uses)) {
            // target is ALWAYS a register!
            count += 1;
        }

        instr++;
        i++;
    }

    return count;
}
