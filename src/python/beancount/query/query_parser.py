"""Parser for Beancount Query Language.
"""
import collections
import itertools

import ply.lex
import ply.yacc


class ParseError(Exception):
    """A parser error."""


class Lexer:
    """Lexer for the Beancount Query Language.
    """

    # List of reserved keywords.
    keywords = {
        'SELECT', 'FROM', 'WHERE',
        # 'GROUP', 'ORDER', 'BY', 'LIMIT', 'DESC', 'ASC',
        # 'AND', 'OR', 'NOT'
    }

    # List of valid tokens from the lexer.
    tokens = ['ID', 'COMMA'] + list(keywords)

    # An identifier, for a target or a dimension or whatever.
    def t_ID(self, token):
        "[a-zA-Z][a-zA-Z_]*"
        utoken = token.value.upper()
        token.type = utoken if utoken in self.keywords else 'ID'
        token.value = token.value.lower()
        return token

    # Constant tokens.
    t_COMMA = ","

    # Ignore whitespace.
    t_ignore = " \t\n"

    # Error handler.
    def t_error(self, token):
        raise ParseError("Unknown token: {}".format(token))


Select = collections.namedtuple('Select',
                                'columns from_clause where_clause')


class Parser:

    # FIXME: Maybe just derivation will work here, try it, try avoiding
    # instantiation of the lexer.
    tokens = Lexer.tokens

    # Starting rule.
    start = 'select_statement'

    def __init__(self, **options):
        self.lexer = create_lexer()
        self.ply_parser = ply.yacc.yacc(module=self,
                                        optimize=False,
                                        write_tables=False,
                                        debug=False,
                                        **options)

    def parse(self, line, debug=False):
        return self.ply_parser.parse(line.strip().rstrip(';'),
                                     lexer=self.lexer,
                                     debug=debug)

    def p_select_statement(self, p):
        """
        select_statement : SELECT id_list opt_from opt_where
        """
        p[0] = Select(p[2], p[3], p[4])

    def p_id_list(self, p):
        """
        id_list : ID
                | id_list COMMA ID
        """
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1]
            p[0].append(p[3])

    def p_opt_from(self, p):
        """
        opt_from : empty
                 | FROM from_expr
        """
        if len(p) > 2:
            p[0] = p[2]

    def p_opt_where(self, p):
        """
        opt_where : empty
                  | WHERE where_expr
        """
        if len(p) > 2:
            p[0] = p[2]

    def p_from_expr(self, p):
        """
        from_expr : empty
        """

    def p_where_expr(self, p):
        """
        where_expr : empty
        """

    def p_empty(self, _):
        """
        empty :
        """

    def p_error(self, context):
        raise ParseError("Syntax error: {}".format(context))


def create_lexer():
    return ply.lex.lex(module=Lexer(), optimize=False)


        #     # Give the lexer some input
        # self.lexer.input(line)

        # # Tokenize
        # while True:
        #     tok = self.lexer.token()
        #     if not tok: break      # No more input
        #     print(tok)
