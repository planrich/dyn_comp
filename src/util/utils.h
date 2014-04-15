
#ifndef _UTILS_H_
#define _UTILS_H_

#include <stdio.h>
#include <stdlib.h>
#include <gc.h>

#define ALLOC_STRUCT(S,V) \
   S * V = malloc(sizeof(S)); \
   if (V == NULL) {               \
       perror("out of memory\n");   \
       exit(1);                     \
   }                                

#define GC_ALLOC_STRUCT(S,V) \
   S * V = (S*) GC_MALLOC(sizeof(S)); \
   if (V == NULL) {               \
       perror("out of memory\n");   \
       exit(1);                     \
   }                                

#define ITER_EXPR_NEXT(node, current, it) \
    { \
        expr_t * it = node; \
        while (it != NULL) { \
            expr_t * current = it;

#define ITER_EXPR_END(it) \
            it = it->next; \
        } \
    }

#define I32_LE_64(i32,arr) \
    arr[0] = i32 & 0xff; \
    arr[1] = (i32 >> 8) & 0xff; \
    arr[2] = (i32 >> 16) & 0xff; \
    arr[3] = (i32 >> 32) & 0xff; \
    arr[4] = 0; \
    arr[5] = 0; \
    arr[6] = 0; \
    arr[7] = 0;
    


#endif /* _UTILS_H_ */
