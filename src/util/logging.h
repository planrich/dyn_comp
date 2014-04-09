
#ifndef _LOGGING_H_
#define _LOGGING_H_

#include <stdio.h>

typedef enum log_level_t {
    LOG_OFF = 0,
    LOG_TRACE = 1,
    LOG_DEBUG = 2,
    LOG_INFO = 4,
    LOG_WARN = 8,
    LOG_FATAL = 16,

    LOG_PARSING = 32,

} log_level_t;

int neart_log_level;

#define NEART_LOG(level, msg, ...) if ((neart_log_level & level) != 0) { printf(msg, ##__VA_ARGS__); }
#define NEART_LOG_ANY(msg, ...) NEART_LOG(0xff, msg, ##__VA_ARGS__)
#define NEART_LOG_INFO(msg, ...) NEART_LOG(LOG_INFO, msg, ##__VA_ARGS__)
#define NEART_LOG_DEBUG(msg, ...) NEART_LOG(LOG_DEBUG, msg, ##__VA_ARGS__)
#define NEART_LOG_FATAL(msg, ...) NEART_LOG(LOG_FATAL, "[fatal] "); NEART_LOG(LOG_FATAL, msg, ##__VA_ARGS__)
#define NEART_LOG_TRACE() NEART_LOG(LOG_TRACE, "[trace] %s\n", __PRETTY_FUNCTION__)

#define NLH NEART_LOG_ANY("here\n");

#define NEART_MSG(msg, ...) printf(msg, ##__VA_ARGS__);

#define IMPL_ME() NEART_LOG_FATAL("(not impl.) func=%s line=%d  file=%s \n", __PRETTY_FUNCTION__, __LINE__, __FILE__)

#endif

