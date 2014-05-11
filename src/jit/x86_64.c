
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
#define X86_PUSH_RAX (0x50)
#define X86_MOV (0xc7)
#define X86_CALLQ (0xff)

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

inline 
void arch_call(memio_t * io, ra_state_t * state, void * func, void * arg1, void * arg2, int time_step) {


    hwreg_t hw_reg = arch_ra_hwreg_any(state);
    // if reg >= r8 -> prepend 0x41
    if (hw_reg >= R8) {
        mem_o_byte(io, 0x41);
    }
    mem_o_byte(io, X86_CALLQ);
    mem_o_byte(io, 0xd0 | (hw_reg % 8));
}

inline 
void arch_ret(memio_t * io) {
    mem_o_byte(io, X86_RET);
}
