#ifndef COLLECTIONS_H
#define COLLECTIONS_H

#include <inttypes.h>
#include "klist.h"
#include "khash.h"

KHASH_MAP_INIT_STR(str_int, uint32_t);

#define __free(x)
KLIST_INIT(32, int32_t, __free);

KLIST_INIT(64, int64_t, __free);

struct __bblock;
KLIST_INIT(bb, struct __bblock*, __free);

#endif
