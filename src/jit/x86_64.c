
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
#define X86_MOV (0xc7)
#define X86_CALLQ (0xff)

#define X86_MOVABS_L (0x48)
#define X86_MOVABS_H (0x49)

#define X86_PUSHQ_BASE (0x50)
#define X86_POPQ_BASE (0x58)

#define X86_PUSH_BYTE (0x6a)
#define X86_PUSH_INT (0x68)

ra_state_t * arch_ra_state_new(void) {
    GC_ALLOC_STRUCT(ra_state_t, s);

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
    f->hw_reg = R8;
    f->range = NULL;
    f = &s->registers[7];
    f->hw_reg = R9;
    f->range = NULL;
    f = &s->registers[8];
    f->hw_reg = R10;
    f->range = NULL;
    f = &s->registers[9];
    f->hw_reg = R11;
    f->range = NULL;
    f = &s->registers[10];
    f->hw_reg = R12;
    f->range = NULL;
    f = &s->registers[11];
    f->hw_reg = R13;
    f->range = NULL;
    f = &s->registers[12];
    f->hw_reg = R14;
    f->range = NULL;
    f = &s->registers[13];
    f->hw_reg = R15;
    f->range = NULL;

    return s;
}

ra_t * _arch_ra_aquire_register(ra_state_t * state, life_range_t * range, vreg_t reg) {

    ra_t * ra = NULL;

    for (int i = 0; i < HW_GP_REG_COUNT; i++) {
        ra = &state->registers[i];
        if (ra->range == NULL) {
            ra->range = range;
            ra->v_reg = reg;
            NEART_LOG_DEBUG("%d %d\n", ra->hw_reg, reg);
            break;
        }
    }

    return ra;
}

int arch_ra_hwreg_in_use(ra_state_t * state, hwreg_t reg) {
    // maybe this can be done in O(1) -> blacklist if register is GP...
    for (int i = 0; i < HW_GP_REG_COUNT; i++) {
        ra_t * ra = &state->registers[i];

        if (ra->hw_reg == reg) {
            return ra->range != NULL;
        }
    }

    return 0;
}

hwreg_t arch_ra_hwreg(ra_state_t * state, life_range_t * ranges, vreg_t reg, int time_step) {

    life_range_t * range = ranges + reg;

    for (int i = 0; i < HW_GP_REG_COUNT; i++) {
        ra_t * ra = &state->registers[i];

        if (ra->range != NULL) {
            if (ra->range->end < time_step) {
                NEART_LOG_DEBUG("releasing reg (mapped v_reg %d) %d at %d\n", ra->v_reg, ra->hw_reg, time_step);
                ra->range = NULL; // release
                continue;
            }
        }

        if (ra->range != NULL && ra->v_reg == reg) {
            NEART_LOG_DEBUG("access hw_reg (mapped v_reg %d) %d at %d\n", ra->v_reg, ra->hw_reg, time_step);
            return ra->hw_reg;
        }
    }

    ra_t * new_reg = _arch_ra_aquire_register(state, range, reg);
    if (new_reg == NULL) {
        return -1;
    }
    NEART_LOG_DEBUG("first access hw_reg (mapped v_reg %d) %d at %d\n", new_reg->v_reg, new_reg->hw_reg, time_step);

    return new_reg->hw_reg;
}

inline
void arch_load_32(memio_t * io, int32_t c, hwreg_t hwreg) {

    mem_o_byte(io, REX | REX_W | (hwreg >= R8 ? REX_B : 0));
    mem_o_byte(io, X86_MOV);
    mem_o_byte(io, MOD_RM(hwreg % 8));
    mem_o_int_le(io, c);
}

int arch_save_hw_reg(memio_t * io, ra_state_t * state, hwreg_t reg) {
    if (arch_ra_hwreg_in_use(state, reg)) {
        if (reg >= R8) {
            mem_o_byte(io, REX | REX_B);
        }

        mem_o_byte(io, X86_PUSHQ_BASE + (reg % 8));

        return 1;
    }
    return 0;
}

void arch_restore_hw_reg(memio_t * io, hwreg_t reg) {
    if (reg >= R8) {
        mem_o_byte(io, REX | REX_B);
    }

    mem_o_byte(io, X86_POPQ_BASE + (reg % 8));
}

void arch_move_long(memio_t * io, hwreg_t reg, void * ptr) {
    if (reg >= R8) {
        mem_o_byte(io, X86_MOVABS_H);
    } else {
        mem_o_byte(io, X86_MOVABS_L);
    }

    mem_o_byte(io, 0xb8 + (reg % 8));
    mem_o_long_le(io, ((int64_t)ptr));
}

inline 
void arch_call(memio_t * io, ra_state_t * state, void * func, void * arg1, void * arg2, int time_step) {

    int hw_reg_in_use = 0;
    hwreg_t hw_reg = R15;

    // save the register if in use
    hw_reg_in_use = arch_save_hw_reg(io, state, hw_reg);

    hwreg_t arg1_reg = RDI;
    hw_reg_in_use |= arch_save_hw_reg(io, state, arg1_reg) << 1;
    hwreg_t arg2_reg = RSI;
    hw_reg_in_use |= arch_save_hw_reg(io, state, arg2_reg) << 2;
    hwreg_t ret_reg = RAX;
    hw_reg_in_use |= arch_save_hw_reg(io, state, ret_reg) << 3;

    arch_move_long(io, hw_reg, func);
    arch_move_long(io, arg1_reg, arg1);
    arch_move_long(io, arg2_reg, arg2);

    // if reg >= r8 -> prepend REX | REX_B 
    if (hw_reg >= R8) {
        mem_o_byte(io, REX | REX_B);
    }
    mem_o_byte(io, X86_CALLQ);
    mem_o_byte(io, 0xd0 | (hw_reg % 8));

    // rax contains the address of the newly compiled code
    // at a later point in time this will not be invoked
    // any more -> the compile function turns this
    // code into a direct call. 
    // load reg, 0xaddr; call reg
    mem_o_byte(io, X86_CALLQ);
    mem_o_byte(io, 0xd0 | (RAX % 8));

    if ((hw_reg_in_use & 0x8) != 0) { arch_restore_hw_reg(io, ret_reg); }
    if ((hw_reg_in_use & 0x4) != 0) { arch_restore_hw_reg(io, arg2_reg); }
    if ((hw_reg_in_use & 0x2) != 0) { arch_restore_hw_reg(io, arg1_reg); }
    if ((hw_reg_in_use & 0x1) != 0) { arch_restore_hw_reg(io, hw_reg); }
}

inline 
void arch_ret(memio_t * io) {
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
