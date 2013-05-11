#include <Python.h>
#include "beancount_lexer.h"
#include "beancount_parser.h"

extern int yydebug;


int main(int argc, char** argv)
{
    int r = 0;

    ++argv, --argc;  /* skip over program name */
    FILE* fp = NULL;
    if ( argc > 0 ) {
        fp = fopen(argv[0], "r");
        yyin = fp;
    }
    else {
        yyin = stdin;
    }

    /* yydebug = 1; */

#if 0
    while ( 1 )  {
        int token = yylex();
        printf("%s = %d\n", getTokenName(token), token);
        if ( token == 0 ) {
            break;
        }
    }
#else
    r = yyparse();
#endif

    if ( fp != NULL ) {
        fclose(fp);
    }
    yylex_destroy();

    Py_Finalize();
    return r;
}
