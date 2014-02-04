
#ifndef _LOGGING_H_
#define _LOGGING_H_

typedef enum log_level_t {
    LOG_OFF = 0,
    LOG_DEBUG = 1,
    LOG_INFO = 2,
    LOG_WARN = 4,
    LOG_FATAL = 8,

    LOG_PARSING = 16

} log_level_t;

static int neart_log_level = LOG_OFF;

#define NEART_LOG(level, msg, args ...) if ((neart_log_level & level) != 0) { printf(msg, args); }
#define NEART_LOG_INFO(msg, args ...) NEART_LOG(LOG_INFO, msg, args)
#define NEART_LOG_ANY(msg, args ...) NEART_LOG(0xff, msg, args)

#endif

