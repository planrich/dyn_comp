#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <errno.h>
#include "ast.h"
#include "neart.tab.h"
#include "logging.h"
#include "config.h"
#include "gpir.h"
#include "code.h"
#include "sem.h"

#ifdef NEART_VISUAL
  #include "dot_syntax_tree.h"
#endif

extern int yyparse();
extern int yylineno;

int yyerror (YYLTYPE *locp, expr_t * root, char const * msg) {
    printf("!> %d:%d %s\n", yylineno, locp->first_column, msg);
    return 1;
}

int main(int argc, char *argv[])
{
    int yy;
    int c;
    int dot_syntax_tree = 0;
    char * dot_file = NULL;
    char * dot_filter_func = NULL;
    char * file = "stdin";

    while (1) {
        int option_index = 0;
        static struct option long_options[] = {
#ifdef NEART_VISUAL
            {"syntax",  required_argument, 0,  0 },
            {"filter",  required_argument, 0,  1 },
#endif
            {"verbose", no_argument,       0,  'v' },
            {0,         0,                 0,  0 }
        };

        c = getopt_long(argc, argv, "v",
                long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
#ifdef NEART_VISUAL
            case 0:
                if (optarg) {
                    dot_file = optarg;
                }
                dot_syntax_tree = 1;
                break;
            case 1:
                if (optarg) {
                    dot_filter_func = optarg;
                }
                break;
#endif
            case 'v':
                neart_log_level = 0xff; // log everything
                break;
        }
    }


    NEART_LOG_INFO("neartc %s-%s\n", NEART_VERSION, NEART_SCM_HASH);

    expr_t * root = neart_expr_alloc(ET_ROOT);

    NEART_LOG(LOG_PARSING, "parsing file '%s'\n", file);
    yy = yyparse(root);
    if (yy != 0) {
        NEART_LOG(LOG_FATAL, "failed at line %d\n", yylineno);
        return 1;
    } else {
        NEART_LOG(LOG_INFO, "parsing '%s' succeded\n", file);
#ifdef NEART_VISUAL
        if (dot_syntax_tree) {
            NEART_LOG(LOG_INFO, "writing syntax tree to file '%s'\n", dot_file);
            neart_write_syntax_tree_to_file(dot_file, root, dot_filter_func);
        }
#endif
    }
    compile_context_t cc;
    errno = 0;
    module_t * module = neart_check_semantics(&cc, root);
    neart_expr_free_r(root);

    if (errno) {
        NEART_LOG(LOG_FATAL, "semantics error found. analysis returned %d\n", errno);
    } else {
        neart_generate_register_code(module, stdout);
    }

    return errno;
}
