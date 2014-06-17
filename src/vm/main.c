
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include "vm.h"
#include "logging.h"
#include "config.h"
#include "compiler.h"
#include "vm.h"
#include "loader.h"
#include "gc.h"
#include "run.h"

int main(int argc, char ** argv) {
    int c;
    int jit = 0;
    clock_t start, end;
    double cpu_time_used;
     

    GC_INIT();

    neart_log_level = 0x0;
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
                neart_log_level = LOG_DEBUG | LOG_INFO;
                break;
            case 'j':
                jit = 1;
                break;
        }
    }

    NEART_LOG_INFO("nvm %s-%s\n", NEART_VERSION, NEART_SCM_HASH);
    if (optind >= argc) {
        printf("usage: nvm [-jv] <rcode_file>\n");
        return EXIT_FAILURE;
    }

    if (access(argv[optind], F_OK) == -1) {
        printf("ncode file '%s' does not exist!\n", argv[optind]);
        return EXIT_FAILURE;
    }

    // use the loader to load the code and the cpool
    vmctx_t * ctx = neart_load_rcode_file(argv[optind]);
    ctx->jit = jit;

    int ret = 0;
    if (jit) {
        start = clock();
        int64_t val = neart_jit_exec(ctx);
        end = clock();
        cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
        printf("register 6 == %lld\n", val);
        jit_print_time();
        printf("took me %f secs\n", cpu_time_used);
    } else {
        start = clock();
        ret = neart_exec(ctx);
        end = clock();
        cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
        printf("took me %f secs\n", cpu_time_used);
        if (ret != 0) {
            NEART_LOG_FATAL("interpretor returned error code %d\n", ret);
        }
        printf("register 6 == %lld\n", ctx->registers[6]);
    }

    ctx = NULL;

    //GC_gcollect();

    return 0;
}
