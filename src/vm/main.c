
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include "vm.h"
#include "logging.h"
#include "config.h"
#include "vm.h"
#include "loader.h"
#include "gc.h"
#include "run.h"

int main(int argc, char ** argv) {
    int c;
    int jit = 0;

    GC_INIT();

    neart_log_level = LOG_INFO;
    while (1) {
        int option_index = 0;
        static struct option long_options[] = {
            {"verbose", no_argument,       0,  'v' },
            {"jit",     no_argument,       0,  'j' },
            {0,         0,                 0,  0 }
        };

        c = getopt_long(argc, argv, "vj",
                long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
            case 'v':
                neart_log_level = LOG_DEBUG; // log everything
                break;
            case 'j':
                jit = 1;
                break;
        }
    }

    NEART_LOG_INFO("nvm %s-%s\n", NEART_VERSION, NEART_SCM_HASH);
    if (optind >= argc) {
        NEART_LOG_FATAL("usage: nvm [-v] <rcode_file>\n");
        return EXIT_FAILURE;
    }

    // use the loader to load the code and the cpool
    vmctx_t * ctx = neart_load_rcode_file(argv[optind]);
    ctx->jit = jit;

    int ret = 0;
    if (jit) {
        ret = neart_jit_exec(ctx);
    } else {
        ret = neart_exec(ctx);
        NEART_LOG_INFO("register 6 == %lld\n", ctx->registers[6]);
    }

    ctx = NULL;

    GC_gcollect();

    return ret;
}
