
#ifndef _UTILS_H_
#define _UTILS_H_


#define ALLOC_STRUCT(S,V) \
   S * V = malloc(sizeof(S)); \
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



#endif /* _UTILS_H_ */
