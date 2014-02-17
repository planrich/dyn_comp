
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include "vm.h"
#include "logging.h"
#include "config.h"
#include "vm.h"

int main(int argc, char ** argv) {
    int c;

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
                //neart_log_level = 0xff; // log everything
                break;
        }
    }


    NEART_LOG_INFO("nvm %s-%s\n", NEART_VERSION, NEART_SCM_HASH);

    /*code_t code[] = {
        NI_SPI, 0x1, 0x0, 0x0, 0x0,
        NI_SPI, 0x2, 0x0, 0x0, 0x0,
        NI_LSI, 0x0,
        NI_LSI, 0x1,
        NR_ADD, 0x0, 0x1, 0x3,
        NR_SUB, 0x3, 0x0, 0x0,
        NR_PUT, 0x0,
        N_END
    };*/

    //if (neart_exec(code)) {
    //    return 0;
    //}

    return 1;
}
