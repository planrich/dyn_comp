#include "cpool_builder.h"


#include "utils.h"
#include <string.h>
#include "logging.h"

int32_t neart_cpool_lookup_symbol(cpool_t * pool, const char * name) {
    uint32_t * wptr = pool->offset_start;
    uint32_t * wptr_end = pool->offset_cursor;
    int32_t index = -1;
    int32_t i = 0;

    //printf("wptr: %lx, wptr_end: %lx, diff: %d\n", wptr, wptr_end, wptr_end - wptr);

    while (wptr < wptr_end) {
        index = *wptr;

        char * sym_name = (char*)(pool->pool_start + index + 4);

        //printf("index is %d & name: %s at addr: %lx -> %lx\n", index, sym_name, pool->pool_start + index, sym_name);

        if (strcmp(name, sym_name) == 0) {
            //printf("lookup at name: %s func: %d\n", name, index);
            return i;
        }

        wptr++;
        i++;
    }

    return -1;
}

int32_t neart_cpool_builder_find_or_reserve_index(cpool_builder_t * cpool, const char * name) {

    int32_t i;
    cpool_t * sym = cpool->sym_pool;
    cpool_t * func = cpool->func_pool;
    uint32_t * cursor = sym->offset_start;

    //printf("cursor: 0x%lx, offset: %d\n", sym->pool_start, 0 );//sym->offset);

    int32_t sym_index = neart_cpool_lookup_symbol(sym, name);
    if (sym_index == -1) {
        //printf("%s not present! => inserting\n", name);
        void * sym_block = neart_cpool_reserve(sym, 4 + strlen(name) + 1, &sym_index);

        int32_t minusOne = -1;
        // reserve function code offset
        int32_t func_index = neart_cpool_insert_int(func, minusOne);

        memcpy(sym_block, &func_index, 4);
        strcpy(sym_block+4, name);

        //printf("inserted at name: %s func: %d sym: %d at addr: %p\n", sym_block+4, func_index, sym_index, sym_block);
    }


    return sym_index;
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
