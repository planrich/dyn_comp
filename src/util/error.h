#ifndef _ERROR_H_
#define _ERROR_H_

typedef enum __semantic_error_t {
    NO_ERROR = 0,
    ERR_FUNC_ALREADY_DEF,

    ERR_SYM_ALREADY_DEF,
    ERR_SYM_NOT_FOUND,
    ERR_SYM_ENTRY_UNKOWN,

    ERR_TYPE_TO_FEW_PARAMETER,
    ERR_TYPE_MATCH_LIST_WITH_NON_LIST,
    ERR_TYPE_MISMATCH,

    ERR_INTERNAL_ENUMERATION_WRONG,
    ERR_INTERNAL_TRANSFORM_ANON_FUNC,
} semantic_error_t;

int neart_error;

#endif /* _ERROR_H_ */
