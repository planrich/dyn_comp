#include "cpool_builder.h"

#include "utils.h"
#include <string.h>

int32_t neart_cpool_builder_find_or_reserve_index(cpool_builder_t * cpool, const char * name) {

    int32_t i;
    cpool_t * sym = cpool->sym_pool;
    cpool_t * func = cpool->func_pool;
    uint32_t * cursor = sym->offset_start;

    // inefficient lookup O(n)
    for (i = 0; i < sym->offset; i++, cursor++) {
        uint32_t * func_pool_idx = sym->pool_start + (*cursor);
        char * sym_name = (char*)(sym->pool_start + (*cursor) + 4);

        if (strcmp(name, sym_name) == 0) {
            printf("lookup at name: %s func: %d\n", name, *func_pool_idx);
            return *func_pool_idx;
        }
    }

    int32_t index = sym->offset;
    void * block = neart_cpool_reserve(sym, 4 + strlen(name) + 1);

    int32_t minusOne = -1;
    // reserve function code offset
    int32_t idx = neart_cpool_insert_int(func, minusOne);

    memcpy(block, &idx, 4);
    strcpy(((char*)block)+4, name);

    printf("inserted at name: %s func: %d sym: %d\n", name, idx, index);

    return idx;
}


cpool_builder_t * neart_cpool_builder_alloc(void) {
    ALLOC_STRUCT(cpool_builder_t, builder);

    builder->sym_pool = neart_cpool_alloc();
    builder->func_pool = neart_cpool_alloc();

    return builder;
}

void neart_cpool_builder_free(cpool_builder_t * bld) {
    neart_cpool_free(bld->sym_pool);
    neart_cpool_free(bld->func_pool);
    free(bld);
}
