#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include "structs.h"
#include "neart.tab.h"
#include "logging.h"
#include "config.h"

extern int yyparse();
extern int yylineno;

int yyerror (YYLTYPE *locp, context_t * ctx, char const * msg) {
    printf("!> %d:%d %s\n", yylineno, locp->first_column, msg);
    return 1;
}

int main(int argc, char *argv[])
{
    int c;
    int digit_optind = 0;
    char * dot_file = NULL;
    int dot_syntax_tree = 0;
    int yy;
    char * file = "stdin";

    while (1) {
        int this_option_optind = optind ? optind : 1;
        int option_index = 0;
        static struct option long_options[] = {
            {"syntax",  required_argument, 0,  0 },
            {"verbose", no_argument, 0,  'v' },
            {0,         0,                 0,  0 }
        };

        c = getopt_long(argc, argv, "v",
                long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
            case 0:
                if (optarg)
                    dot_file = optarg;
                dot_syntax_tree = 1;
                break;
            case 'v':
                neart_log_level = 0xff; // log everything
                break;
        }
    }


    NEART_LOG_INFO("neartc %s-%s\n", NEART_VERSION, NEART_SCM_HASH);


    context_t * ctx = neart_context_alloc(file);

    NEART_LOG(LOG_PARSING, "parsing file '%s'\n", file);
    yy = yyparse(ctx);
    if (yy != 0)
    {
        NEART_LOG(LOG_FATAL, "failed at line %d\n", yylineno);
        return 1;
    }
    else{
        NEART_LOG(LOG_INFO, "parsing '%s' succeded\n", file);
        if (dot_syntax_tree) {
            NEART_LOG(LOG_INFO, "writing syntax tree to file '%s'\n", dot_file);
            neart_write_syntax_tree_to_file(file, ctx->syntax_tree);
        }
    }

    neart_context_free(ctx);

    return 0;
}
