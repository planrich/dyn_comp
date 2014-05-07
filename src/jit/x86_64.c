
#include "x86_64.h"

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
#define X86_CALL (0xe8)

inline
hwreg_t arch_ra_hwreg(ra_state_t * state, life_range_t * ranges, vreg_t reg) {



    return R15;
}

inline
void arch_load_32(memio_t * io, int32_t c, hwreg_t hwreg) {

    mem_o_byte(io, REX | REX_W | (hwreg >= R8 ? REX_B : 0));
    mem_o_byte(io, X86_MOV);
    mem_o_byte(io, MOD_RM(hwreg % 8));
    mem_o_int_le(io, c);
}

inline arch_call(memio_t * io, void * arg1, void * arg2) {
    //
}

inline 
void arch_ret(memio_t * io) {
    mem_o_byte(io, X86_RET);
}
