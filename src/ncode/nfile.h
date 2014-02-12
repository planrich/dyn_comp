#ifndef _NFILE_H_
#define _NFILE_H_

#include "code.h"

typedef struct __nfile_t {
    const char * name;
    cpool * pool;
    code_t * code;
} nfile_t;

nfile_t * neart_read(const char * name);

#endif /* _NFILE_H_ */
