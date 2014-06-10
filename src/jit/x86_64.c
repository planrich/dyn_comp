
#include "x86_64.h"

#include "utils.h"
#include "logging.h"

static mcode_t mbuf[16];
#define MOD_RM(p) ((3 << 6) | p)
#define LE_64_BIT(i32)

// register extention
#define REX (0x40)
#define REX_W (0x8)
#define REX_R (0x4)
#define REX_X (0x2)
#define REX_B (0x1)

#define X86_RET (0xc3)
#define X86_LEAVE (0xc9)
#define X86_MOV (0xc7)
#define X86_MOV_REG_REG (0x89)
#define X86_MOV_REG_OR_OFF_TO_REG (0x8b)
#define X86_CALLQ (0xff)

#define X86_OP_81 (0x81)
#define X86_ADD_REG_REG (0x01)
#define X86_SUB_REG_REG (0x29)

#define X86_MOVABS_L (0x48)
#define X86_MOVABS_H (0x49)

#define X86_PUSHQ_BASE (0x50)
#define X86_POPQ_BASE (0x58)

#define X86_PUSH_BYTE (0x6a)
#define X86_PUSH_INT (0x68)

#define X86_JMP_REL (0xe9)

#define FAR_JMP (0x0f)
#define X86_JMP_EQ (0x84)

#define X86_CMP_REG_REG (0x39)

/**
 * ARG 0 = RDI
 * ARG 1 = RSI
 * ...
 * this function maps them
 */
static
qw_registers_t _arch_arg_to_c_reg(ra_state_t * state, int arg_reg);

static 
ra_t * _arch_ra_aquire_register(ra_state_t * state, life_range_t * range, vreg_t reg);

/**
 * Move assembler instruction to io.
 */
static
void _arch_move_hwreg(memio_t * io, hwreg_t hw_source, hwreg_t hw_target);

/**
 * Cmp two registers.
 */
static
void _arch_cmp_hwreg(memio_t * io, hwreg_t hw_source, hwreg_t hw_target);

/**
 * Add two registers into the target.
 */
static
void _arch_add_hwreg(memio_t * io, hwreg_t hw_source, hwreg_t hw_target);


/**
 * Substract two registers into the target.
 */
static
void _arch_sub_hwreg(memio_t * io, hwreg_t hw_source, hwreg_t hw_target);

/**
 * Call a routine thats' address is in a register.
 */
static
void _arch_call_reg(memio_t * io, hwreg_t hw_reg);

inline
void arch_cond_jmp(memio_t * io, ra_state_t * state, vreg_t s1, vreg_t s2, int32_t diff, neart_instr_codes_t code) {
    hwreg_t hw_s1 = arch_ra_hwreg(state, s1);
    hwreg_t hw_s2 = arch_ra_hwreg(state, s2);

    _arch_cmp_hwreg(io, hw_s1, hw_s2);

    mem_o_byte(io, FAR_JMP);
    switch (code) {
        case NR_SKIP_EQ:
            mem_o_byte(io, X86_JMP_EQ);
            break;
        default:
            IMPL_ME();
    }

    // reserve space for real byte offset
    mem_o_int_le(io, diff);
}

inline
void arch_jmp(memio_t * io, int32_t diff) {
    mem_o_byte(io, X86_JMP_REL);
    // reserve space for real byte offset
    mem_o_int_le(io, diff);
}

inline
void arch_patch_jmp(memio_t * io, neart_instr_codes_t code, mcode_t * mcode) {
    mcode_t * start;
    int offset = 0;
    switch (code) {
        case NR_SKIP_EQ:
            offset = 3 + 2; // 3 cmp, 2 head je
            start = io->memory + 3 + 6;
            break;
        case NR_JMP:
            offset = 1;
            start = io->memory + 5;
            break;
        default:
            IMPL_ME();
    }
    io->cursor = offset;

    int diff = mcode - start;

    mem_o_int_le(io, diff);
}


inline
void _arch_cmp_hwreg(memio_t * io, hwreg_t hw_source, hwreg_t hw_target) {
    mem_o_byte(io, REX | REX_W | (hw_target >= R8 ? REX_B : 0) | (hw_source >= R8 ? REX_R : 0) );
    mem_o_byte(io, X86_CMP_REG_REG);
    mem_o_byte(io, MOD_RM(hw_source * 8 | (hw_target & 0x7)));
}

inline
void _arch_move_hwreg(memio_t * io, hwreg_t hw_source, hwreg_t hw_target) {
    mem_o_byte(io, REX | REX_W | (hw_target >= R8 ? REX_B : 0) | (hw_source >= R8 ? REX_R : 0) );
    mem_o_byte(io, X86_MOV_REG_REG);
    mem_o_byte(io, MOD_RM(hw_source * 8 | (hw_target & 0x7)));
}

void arch_move_reg(memio_t * io, ra_state_t * state, vreg_t source, vreg_t target) {
    hwreg_t hw_source = arch_ra_hwreg(state, source);
    hwreg_t hw_target = arch_ra_hwreg(state, target);

    _arch_move_hwreg(io, hw_source, hw_target);
}

qw_registers_t _arch_arg_to_c_reg(ra_state_t * state, int arg_reg) {
    ra_t * ra = &state->registers[state->reg_displacement[HW_GP_REG_COUNT + arg_reg]];
    return ra->hw_reg;
}

void arch_mov_arg_reg_to_stack(memio_t * io, ra_state_t * state, int arg_reg) {
    hwreg_t hw_source = _arch_arg_to_c_reg(state, arg_reg);
    hwreg_t hw_target = RBP;
    char off = -((arg_reg+1) * 8); // validate arg_reg or add +1 ?
    mem_o_byte(io, REX | REX_W | (hw_target >= R8 ? REX_B : 0) | (hw_source >= R8 ? REX_R : 0) );
    mem_o_byte(io, X86_MOV_REG_REG);
    mem_o_byte(io, 0x40 | (hw_source * 8) | (hw_target & 0x7));
    mem_o_byte(io, off);
}

void arch_mov_arg_stack_to_reg(memio_t * io, ra_state_t * state, int arg_reg, vreg_t vreg) {
    hwreg_t hw_source = arch_ra_hwreg(state, vreg); // _arch_arg_to_c_reg(state, arg_reg);
    hwreg_t hw_target = RBP;
    char off = -((arg_reg+1) * 8); // validate arg_reg or add +1 ?
    mem_o_byte(io, REX | REX_W | (hw_target >= R8 ? REX_B : 0) | (hw_source >= R8 ? REX_R : 0) );
    mem_o_byte(io, X86_MOV_REG_OR_OFF_TO_REG);
    mem_o_byte(io, 0x40 | (hw_source * 8) | (hw_target & 0x7));
    mem_o_byte(io, off);
}

inline
void _arch_sub_hwreg(memio_t * io, hwreg_t hw_source, hwreg_t hw_target) {
    mem_o_byte(io, REX | REX_W | (hw_target >= R8 ? REX_B : 0) | (hw_source >= R8 ? REX_R : 0) );
    mem_o_byte(io, X86_SUB_REG_REG);
    mem_o_byte(io, MOD_RM((hw_source * 8) | hw_target & 0x7));
}

inline
void _arch_add_hwreg(memio_t * io, hwreg_t hw_source, hwreg_t hw_target) {
    mem_o_byte(io, REX | REX_W | (hw_target >= R8 ? REX_B : 0) | (hw_source >= R8 ? REX_R : 0) );
    mem_o_byte(io, X86_ADD_REG_REG);
    mem_o_byte(io, MOD_RM((hw_source * 8) | hw_target & 0x7));
}

void arch_sub_reg(memio_t * io, ra_state_t * state, vreg_t s1, vreg_t s2, vreg_t target) {
    hwreg_t hw_s1 = arch_ra_hwreg(state, s1);
    hwreg_t hw_s2 = arch_ra_hwreg(state, s2);
    hwreg_t hw_target = arch_ra_hwreg(state, target);

    _arch_sub_hwreg(io, hw_s1, hw_s2);
    _arch_move_hwreg(io, hw_s2, hw_target);
}

void arch_add_reg(memio_t * io, ra_state_t * state, vreg_t s1, vreg_t s2, vreg_t target) {
    hwreg_t hw_s1 = arch_ra_hwreg(state, s1);
    hwreg_t hw_s2 = arch_ra_hwreg(state, s2);
    hwreg_t hw_target = arch_ra_hwreg(state, target);

    _arch_add_hwreg(io, hw_s1, hw_s2);
    _arch_move_hwreg(io, hw_s2, hw_target);
}

void arch_move_long(memio_t * io, hwreg_t reg, int64_t value) {
    if (reg >= R8) {
        mem_o_byte(io, X86_MOVABS_H);
    } else {
        mem_o_byte(io, X86_MOVABS_L);
    }

    mem_o_byte(io, 0xb8 + (reg % 8));
    mem_o_long_le(io, value);
}

inline 
void _arch_call_reg(memio_t * io, hwreg_t hw_reg) {
    if (hw_reg > R8) {
        mem_o_byte(io, REX | REX_B);
    }
    mem_o_byte(io, X86_CALLQ);
    mem_o_byte(io, 0xd0 | (hw_reg % 8));
}
inline 
void arch_call(memio_t * io, ra_state_t * state, void * func, void * arg1) {

    hwreg_t hw_reg = R15;
    hwreg_t arg1_reg = RDI;
    hwreg_t ret_reg = RAX;

    arch_save_hw_reg(io, state, hw_reg);
    arch_save_hw_reg(io, state, arg1_reg);
    arch_save_hw_reg(io, state, RSI);
    arch_save_hw_reg(io, state, ret_reg);

    arch_move_long(io, hw_reg, (int64_t)func);
    arch_move_long(io, arg1_reg, (int64_t)arg1);

    // if reg >= r8 -> prepend REX | REX_B 
    if (hw_reg >= R8) {
        mem_o_byte(io, REX | REX_B);
    }
    mem_o_byte(io, X86_CALLQ);
    mem_o_byte(io, 0xd0 | (hw_reg % 8));

    _arch_move_hwreg(io, RAX, hw_reg);

    arch_restore_hw_reg(io, ret_reg);

    arch_restore_hw_reg(io, RSI);
    arch_restore_hw_reg(io, arg1_reg);

    // rax contains the address of the newly compiled code
    // at a later point in time this will not be invoked
    // any more -> the compile function turns this
    // code into a direct call. 
    // load reg, 0xaddr; call reg
    _arch_call_reg(io, hw_reg);

    arch_restore_hw_reg(io, hw_reg);
}

inline
void arch_replace_jit_call(memio_t * io, void * mcode_addr, void * end_ptr) {
    hwreg_t reg = R15;
    arch_move_long(io, reg, (int64_t)mcode_addr);
    int diff = end_ptr - (io->memory + io->cursor) - 5; // minus 5. jmp <int> -> 5 bytes.

    mem_o_byte(io, X86_JMP_REL);
    mem_o_int_le(io, diff);
}

inline 
void arch_ret(memio_t * io, int var_byte_count) {

    arch_restore_hw_reg(io, RBX);
    arch_restore_hw_reg(io, R8);
    arch_restore_hw_reg(io, R9);
    arch_restore_hw_reg(io, R10);
    arch_restore_hw_reg(io, R11);
    arch_restore_hw_reg(io, R12);
    arch_restore_hw_reg(io, R13);
    arch_restore_hw_reg(io, R14);
    arch_restore_hw_reg(io, R15);

    /*if (var_byte_count > 0) {
        mem_o_byte(io, REX | REX_W);
        mem_o_byte(io, X86_OP_81);
        mem_o_byte(io, 0xc0 + RSP); // c0 add?
        mem_o_int_le(io, var_byte_count);
    }

    // mov rbp -> rsp
    // NO NEED to do that! 
*/
    _arch_move_hwreg(io, RBP, RSP);

    arch_restore_hw_reg(io, RBP);
    //mem_o_byte(io, X86_LEAVE);
    mem_o_byte(io, X86_RET);
}

void arch_push_const(memio_t * io, int32_t c) {
    /* hard to use later -> only use 32 bit int instead
    if (((uint32_t)c) <= 0xff) {
        mem_o_byte(io, X86_PUSH_BYTE);
        mem_o_byte(io, c & 0xff);
    } else {*/
        mem_o_byte(io, X86_PUSH_INT);
        mem_o_int_le(io, c);
    /*}*/
}

void arch_enter_routine(memio_t * io, int32_t var_count_bytes) {
    // push rbp
    mem_o_byte(io, X86_PUSHQ_BASE + RBP);

    // mov rsp -> rbp
    mem_o_byte(io, REX | REX_W);
    mem_o_byte(io, X86_MOV_REG_REG);
    mem_o_byte(io, MOD_RM(RSP * 8 | (RBP & 0x7)));
    if (var_count_bytes > 0) {
        mem_o_byte(io, REX | REX_W);
        mem_o_byte(io, X86_OP_81);
        mem_o_byte(io, 0xe8 + RSP); // 0xec
        mem_o_int_le(io, var_count_bytes);
    }

    arch_save_hw_reg(io, NULL, R15);
    arch_save_hw_reg(io, NULL, R14);
    arch_save_hw_reg(io, NULL, R13);
    arch_save_hw_reg(io, NULL, R12);
    arch_save_hw_reg(io, NULL, R11);
    arch_save_hw_reg(io, NULL, R10);
    arch_save_hw_reg(io, NULL, R9);
    arch_save_hw_reg(io, NULL, R8);
    arch_save_hw_reg(io, NULL, RBX);
}

inline
void arch_load_32(memio_t * io, int32_t c, hwreg_t hwreg) {

    mem_o_byte(io, REX | REX_W | (hwreg >= R8 ? REX_B : 0));
    mem_o_byte(io, X86_MOV);
    mem_o_byte(io, MOD_RM(hwreg % 8));
    mem_o_int_le(io, c);
}

void arch_save_hw_reg(memio_t * io, ra_state_t * state, hwreg_t reg) {
    if (reg >= R8) {
        mem_o_byte(io, REX | REX_B);
    }
    mem_o_byte(io, X86_PUSHQ_BASE + (reg % 8));
}

void arch_restore_hw_reg(memio_t * io, hwreg_t reg) {
    if (reg >= R8) {
        mem_o_byte(io, REX | REX_B);
    }

    mem_o_byte(io, X86_POPQ_BASE + (reg % 8));
}

ra_state_t * arch_ra_state_new(void) {
    GC_ALLOC_STRUCT(ra_state_t, s);

    // why this way? this is really not maintainable...
    // fix this... maybe some time later :)
    ra_t * f;
    f = &s->registers[0];
    f->hw_reg = RAX;
    f->range = NULL;
    f = &s->registers[1];
    f->hw_reg = RCX;
    f->range = NULL;
    f = &s->registers[2];
    f->hw_reg = RDX;
    f->range = NULL;
    f = &s->registers[3];
    f->hw_reg = RBX;
    f->range = NULL;
    f = &s->registers[4];
    f->hw_reg = RSI;
    f->range = NULL;
    f = &s->registers[5];
    f->hw_reg = RDI;
    f->range = NULL;
    f = &s->registers[6];
    f->hw_reg = RBP;
    f->range = NULL;
    f = &s->registers[7];
    f->hw_reg = RSP;
    f->range = NULL;
    f = &s->registers[8];
    f->hw_reg = R8;
    f->range = NULL;
    f = &s->registers[9];
    f->hw_reg = R9;
    f->range = NULL;
    f = &s->registers[10];
    f->hw_reg = R10;
    f->range = NULL;
    f = &s->registers[11];
    f->hw_reg = R11;
    f->range = NULL;
    f = &s->registers[12];
    f->hw_reg = R12;
    f->range = NULL;
    f = &s->registers[13];
    f->hw_reg = R13;
    f->range = NULL;
    f = &s->registers[14];
    f->hw_reg = R14;
    f->range = NULL;
    f = &s->registers[15];
    f->hw_reg = R15;
    f->range = NULL;

    s->reg_displacement[0] = R15;
    s->reg_displacement[1] = R14;
    s->reg_displacement[2] = R13;
    s->reg_displacement[3] = R12;
    s->reg_displacement[4] = R11;
    s->reg_displacement[5] = R10;
    s->reg_displacement[6] = 3;

    s->reg_displacement[7] = 5;
    s->reg_displacement[8] = 4;
    s->reg_displacement[9] = 2;
    s->reg_displacement[10] = 1;
    s->reg_displacement[11] = 8;
    s->reg_displacement[12] = 9;
    s->reg_displacement[13] = RAX;

    return s;
}

hwreg_t arch_ra_hwreg(ra_state_t * state, vreg_t reg) {

    int time_step = state->time_step;
    life_range_t * range = state->ranges + reg;

    if (reg > 6) {
        for (int i = 0; i < HW_GP_REG_COUNT; i++) {
            ra_t * ra = &state->registers[state->reg_displacement[i]];

            if (ra->range != NULL) {
                if (ra->range->end < time_step) {
                    //NEART_LOG_DEBUG("releasing reg (mapped v_reg %d) %d at %d\n", ra->v_reg, ra->hw_reg, time_step);
                    ra->range = NULL; // release
                    continue;
                }
            }

            if (ra->range != NULL && ra->v_reg == reg) {
                //NEART_LOG_DEBUG("access hw_reg (mapped v_reg %d) %d at %d\n", ra->v_reg, ra->hw_reg, time_step);
                return ra->hw_reg;
            }
        }
    }

    ra_t * new_reg = _arch_ra_aquire_register(state, range, reg);
    if (new_reg == NULL) {
        NEART_LOG_FATAL("oh no! i ran out of registers... this would be a good time to spill\n");
        return -1;
    }
    //NEART_LOG_DEBUG("first access hw_reg (mapped v_reg %d) %d at %d\n", new_reg->v_reg, new_reg->hw_reg, time_step);

    return new_reg->hw_reg;
}

ra_t * _arch_ra_aquire_register(ra_state_t * state, life_range_t * range, vreg_t reg) {

    ra_t * ra = NULL;

    int pos = 0;
    // it is a special register -> only use the real mapping
    if (reg <= 6) {
        pos = state->reg_displacement[HW_GP_REG_COUNT + reg];
        ra = &state->registers[pos];
        if (ra->range != NULL) {
            //IMPL_ME();
            //printf("fail fail fail r%d, start: %d, end: %d now: %d\n", range->reg, range->start, range->end, state->time_step);
        } else {
            ra->range = range;
            ra->v_reg = reg;
            //NEART_LOG_DEBUG("allocating hw: %d with virtual: %d\n", ra->hw_reg, reg);
        }
        return ra;
    }

    for (int i = 0; i < HW_GP_REG_COUNT; i++) {
        ra = &state->registers[state->reg_displacement[i]];
        if (ra->range == NULL) {
            ra->range = range;
            ra->v_reg = reg;
            //NEART_LOG_DEBUG("allocating hw: %d with virtual: %d\n", ra->hw_reg, reg);
            break;
        }
        ra = NULL;
    }

    return ra;
}

int arch_ra_hwreg_in_use(ra_state_t * state, hwreg_t reg) {
    // maybe this can be done in O(1) -> blacklist if register is GP...
    for (int i = 0; i < HW_REG_COUNT; i++) {
        ra_t * ra = &state->registers[i];

        if (ra->hw_reg == reg) {
            return ra->range != NULL && ra->range->end <= state->time_step;
        }
    }

    return 0;
}
