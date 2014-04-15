#ifndef REGISTER_ALLOCATOR_H
#define REGISTER_ALLOCATOR_H

#include "collections.h"
#include "vm.h"

void n_rallocator(rcode_t * code, rcode_t * end, klist_t(32) assignment);

#endif
