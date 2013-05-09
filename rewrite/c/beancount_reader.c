#include "beancount_lexer.h"
#include "beancount_parser.h"

const char* getTokenName(int token);

extern int yydebug;

int main(int argc, char** argv)
{
    ++argv, --argc;  /* skip over program name */
    if ( argc > 0 )
        yyin = fopen(argv[0], "r");
    else
        yyin = stdin;

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
    int r = yyparse();
    return r;
#endif
}
