
#ifndef _UTILS_H_
#define _UTILS_H_


#define ALLOC_STRUCT(structure,var) \
   structure * var = malloc(sizeof(structure)); \
   if (var == NULL) {               \
       perror("out of memory\n");   \
       exit(1);                     \
   }                                \



#endif /* _UTILS_H_ */
