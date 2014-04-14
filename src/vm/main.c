
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include "vm.h"
#include "logging.h"
#include "config.h"
#include "vm.h"
#include "loader.h"
#include "gc.h"

int main(int argc, char ** argv) {
    int c;

    GC_INIT();

    neart_log_level = LOG_INFO;
    while (1) {
        int option_index = 0;
        static struct option long_options[] = {
            {"verbose", no_argument,       0,  'v' },
            {0,         0,                 0,  0 }
        };

        c = getopt_long(argc, argv, "v",
                long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
            case 'v':
                neart_log_level = LOG_DEBUG; // log everything
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

    int ret = neart_exec(ctx);

    NEART_LOG_INFO("register 6 == %lld\n", ctx->registers[6]);

    return 0;
}
