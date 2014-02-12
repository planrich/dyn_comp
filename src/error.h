#ifndef _ERROR_H_
#define _ERROR_H_

typedef enum __semantic_error_t {
    ERR_FUNC_ALREADY_DEF,
    ERR_SYM_ALREADY_DEF,
    ERR_INTERNAL_ENUMERATION_WRONG,
    ERR_SYM_NOT_FOUND,
    ERR_SYM_ENTRY_UNKOWN,
} semantic_error_t;

int neart_error;

#endif /* _ERROR_H_ */
