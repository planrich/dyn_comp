
#include "x86_64.h"

static mcode_t mbuf[16];
#define OPCODE(oc) *__mc++ = oc
#define PREFIX(p) OPCODE(p)
#define MOD_RM(p) OPCODE((3 << 6) | p)
#define LE_64_BIT(i32)

// register extention
#define REX (0x40)
#define REX_W (0x8)
#define REX_R (0x4)
#define REX_X (0x2)
#define REX_B (0x1)


inline
void arch_load_32(mcode_t * __mc, rcode_t * data, qw_registers_t reg) {

    int32_t i = *(int32_t)data;

    PREFIX(REX | REX_W | (reg >= R8 ? REX_B : 0));
    OPCODE(0xc7);
    MOD_RM(reg % 8);

    I32_LE_64(i, __mc);

    __mc += 8;
}
