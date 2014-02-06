
#ifndef _UTILS_H_
#define _UTILS_H_


#define ALLOC_STRUCT(S,V) \
   S * V = malloc(sizeof(S)); \
   if (V == NULL) {               \
       perror("out of memory\n");   \
       exit(1);                     \
   }                                



#endif /* _UTILS_H_ */
