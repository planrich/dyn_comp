#ifndef COLLECTIONS_H
#define COLLECTIONS_H

#include "klist.h"
#include "khash.h"

KHASH_MAP_INIT_STR(str_int, uint32_t);

#define __free(x)
KLIST_INIT(32, int32_t, __free);

KLIST_INIT(tuple32, uint64_t, __free);

#endif
