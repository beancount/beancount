"""Parser for Beancount Query Language.
"""
import collections
import datetime
import itertools
import io
import re

import dateutil.parser

import ply.lex
import ply.yacc

from beancount.core.amount import D
from beancount.core import position
from beancount.utils.misc_utils import cmptuple


# A 'select' query action.
#
# Attributes:
#   targets: Either a single 'Wildcard' instance of a list of 'Target'
#     instances.
#   from_clause: An instance of 'From', or None if absent.
#   where_clause: A root expression node, or None if absent.
#   group_by: An instance of 'GroupBy', or None if absent.
#   order_by: An instance of 'OrderBy', or None if absent.
#   pivot_by: An instance of 'PivotBy', or None if absent.
#   limit: An integer, or None is absent.
#   distinct: A boolean value (True), or None if absent.
#   flatten: A boolean value (True), or None if absent.
Select = collections.namedtuple(
    'Select', ('targets from_clause where_clause '
               'group_by order_by pivot_by limit distinct flatten'))

# A select query that produces final balances for accounts.
# This is equivalent to
#
#   SELECT account, sum(change) FROM <from_clause>
#   GROUP BY account
#
# Attributes:
#   from_clause: An instance of 'From', or None if absent.
Balance = collections.namedtuple('Balance', 'from_clause')


# A select query that produces a journal of postings.
# This is equivalent to
#
#   SELECT date, flag, payee, narration, ...  FROM <from_clause>
#   WHERE account = <account>
#
# Attributes:
#   account: A string, the name of the account to restrict to.
#   from_clause: An instance of 'From', or None if absent.
Journal = collections.namedtuple('Journal', 'account from_clause')


# AST nodes for the top-level clauses.
Target = cmptuple('Target', 'expression name')
Wildcard = cmptuple('Wildcard', '')
From = cmptuple('From', 'expression close')
GroupBy = cmptuple('GroupBy', 'columns having')
OrderBy = cmptuple('OrderBy', 'columns ordering')
PivotBy = cmptuple('PivotBy', 'columns')

# Nodes used in expressions. The meaning should be self-explanatory. This is
# your run-of-the-mill hierarchical logical expression nodes. Any of these nodes
# equivalent form "an expression."
Column = cmptuple('Column', 'name')
Function = cmptuple('Function', 'fname operands')
Constant = cmptuple('Constant', 'value')
UnaryOp = cmptuple('UnaryOp', 'operand')
class Not(UnaryOp): pass
BinaryOp = cmptuple('BinaryOp', 'left right')
class Equal(BinaryOp): pass
class Match(BinaryOp): pass
class And(BinaryOp): pass
class Or(BinaryOp): pass
class Greater(BinaryOp): pass
class GreaterEq(BinaryOp): pass
class Less(BinaryOp): pass
class LessEq(BinaryOp): pass



class ParseError(Exception):
    """A parser error."""


class Lexer:
    """PLY lexer for the Beancount Query Language.
    """

    # List of reserved keywords.
    keywords = {
        'SELECT', 'AS', 'FROM', 'WHERE', 'CLOSE', 'ON',
        'BALANCE', 'JOURNAL',
        'GROUP', 'BY', 'HAVING', 'ORDER', 'DESC', 'ASC', 'PIVOT',
        'LIMIT', 'FLATTEN', 'DISTINCT',
        'AND', 'OR', 'NOT', 'GT', 'GTE', 'LT', 'LTE',
        'TRUE', 'FALSE', 'NULL',
    }

    # List of valid tokens from the lexer.
    tokens = [
        'ID', 'INTEGER', 'DECIMAL', 'STRING', 'DATE',
        'WILDCARD', 'COMMA', 'SEMI', 'LPAREN', 'RPAREN', 'TILDE',
        'EQ', 'NE',
    ] + list(keywords)

    # An identifier, for a column or a dimension or whatever.
    def t_ID(self, token):
        "[a-zA-Z][a-zA-Z_]*"
        utoken = token.value.upper()
        if utoken in self.keywords:
            token.type = utoken
            token.value = token.value.upper()
        else:
            token.value = token.value.lower()
        return token

    def t_STRING(self, token):
        "(\"[^\"]*\"|\'[^\']*\')"
        token.value = token.value[1:-1]
        return token

    def t_DATE(self, token):
        r"(\#(\"[^\"]*\"|\'[^\']*\')|\d\d\d\d-\d\d-\d\d)"
        if token.value[0] == '#':
            token.value = dateutil.parser.parse(token.value[2:-1]).date()
        else:
            token.value = datetime.datetime.strptime(token.value, '%Y-%m-%d').date()
        return token

    # Constant tokens.
    t_WILDCARD  = r"\*"
    t_COMMA  = r","
    t_SEMI   = r";"
    t_LPAREN = r"\("
    t_RPAREN = r"\)"
    t_NE     = r"!="
    t_EQ     = r"="
    t_GTE  = r">="
    t_GT  = r">"
    t_LTE  = r"<="
    t_LT  = r"<"
    t_TILDE  = r"~"

    # Numbers.
    def t_DECIMAL(self, token):
        r"[0-9]*\.[0-9]*"
        token.value = D(token.value)
        return token

    def t_INTEGER(self, token):
        r"[0-9]+"
        token.value = int(token.value)
        return token

    # Ignore whitespace.
    t_ignore = " \t\n"

    # Error handler.
    def t_error(self, token):
        raise ParseError("Unknown token: {}".format(token))


class Parser(Lexer):
    """PLY parser for the Beancount Query Language.
    """

    # Starting rule.
    start = 'statement'

    def __init__(self, **options):
        self.ply_lexer = ply.lex.lex(module=self,
                                     optimize=False)
        self.ply_parser = ply.yacc.yacc(module=self,
                                        optimize=False,
                                        write_tables=False,
                                        debugfile=None,
                                        debug=False,
                                        **options)

    def tokenize(self, line):
        self.ply_lexer.input(line)
        while 1:
            tok = self.ply_lexer.token()
            if not tok:
                break
            print(tok)

    def parse(self, line, debug=False):
        try:
            self._input = line
            return self.ply_parser.parse(line,
                                         lexer=self.ply_lexer,
                                         debug=debug)
        finally:
            self._input = None

    def handle_comma_separated_list(self, p):
        """Handle a list of 0, 1 or more comma-separated values.
        Args:
          p: A gramar object.
        """
        if len(p) == 2:
            return [] if p[1] is None else [p[1]]
        else:
            return p[1] + [p[3]]

    def p_statement(self, p):
        """
        statement : select_statement SEMI
                  | balance_statement SEMI
                  | journal_statement SEMI
        """
        p[0] = p[1]

    def p_balance_statement(self, p):
        """
        balance_statement : BALANCE from
        """
        p[0] = Balance(p[2])

    def p_journal_statement(self, p):
        """
        journal_statement : JOURNAL from
                          | JOURNAL account from
        """
        p[0] = Journal(p[2], p[3]) if len(p) == 4 else Journal(None, p[2])

    def p_account(self, p):
        """
        account : STRING
        """
        p[0] = p[1]

    def p_select_statement(self, p):
        """
        select_statement : SELECT distinct target_spec from_subselect where \
                           group_by order_by pivot_by limit flatten
        """
        p[0] = Select(p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[2], p[10])

    def p_distinct(self, p):
        """
        distinct : empty
                 | DISTINCT
        """
        p[0] = True if p[1] == 'DISTINCT' else None

    def p_target_spec(self, p):
        """
        target_spec : WILDCARD
                    | target_list
        """
        p[0] = Wildcard() if p[1] == '*' else p[1]

    def p_target_list(self, p):
        """
        target_list : target
                    | target_list COMMA target
        """
        p[0] = self.handle_comma_separated_list(p)

    def p_target(self, p):
        """
        target : expression AS ID
               | expression
        """
        p[0] = Target(p[1], p[3] if len(p) == 4 else None)

    def p_from_empty(self, p):
        "from : empty"
        p[0] = None

    def p_from_expression_only(self, p):
        "from : FROM expression"
        p[0] = From(p[2], False)

    def p_from_close_only(self, p):
        "from : FROM close"
        p[0] = From(None, p[2])

    def p_from_expression_close(self, p):
        "from : FROM expression close"
        p[0] = From(p[2], p[3])

    def p_from_subselect(self, p):
        """
        from_subselect : from
                       | FROM LPAREN select_statement RPAREN
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = p[3]

    def p_close(self, p):
        """
        close : CLOSE
              | CLOSE ON DATE
        """
        p[0] = p[3] if len(p) == 4 else (p[1] == 'CLOSE')

    def p_where(self, p):
        """
        where : empty
              | WHERE expression
        """
        if len(p) == 3:
            assert p[2], "Empty WHERE clause is not allowed"
            p[0] = p[2]

    def p_group_by(self, p):
        """
        group_by : empty
                 | GROUP BY column_list having
        """
        p[0] = GroupBy(p[3], p[4]) if len(p) != 2 else None

    def p_having(self, p):
        """
        having : empty
               | HAVING expression
        """
        p[0] = p[2] if len(p) == 3 else None

    def p_order_by(self, p):
        """
        order_by : empty
                 | ORDER BY column_list ordering
        """
        p[0] = None if len(p) == 2 else OrderBy(p[3], p[4])

    def p_ordering(self, p):
        """
        ordering : empty
                 | ASC
                 | DESC
        """
        p[0] = p[1]

    def p_pivot_by(self, p):
        """
        pivot_by : empty
                 | PIVOT BY column_list
        """
        p[0] = PivotBy(p[3]) if len(p) == 4 else None

    def p_limit(self, p):
        """
        limit : empty
              | LIMIT INTEGER
        """
        p[0] = p[2] if len(p) == 3 else None

    def p_flatten(self, p):
        """
        flatten : empty
                | FLATTEN
        """
        p[0] = True if p[1] == 'FLATTEN' else None


    precedence = [
        ('left', 'OR'),
        ('left', 'AND'),
        ('left', 'NOT'),
        ('left', 'EQ', 'NE', 'GT', 'GTE', 'LT', 'LTE', 'TILDE'),
        ]

    def p_expression_and(self, p):
        "expression : expression AND expression"
        p[0] = And(p[1], p[3])

    def p_expression_or(self, p):
        "expression : expression OR expression"
        p[0] = Or(p[1], p[3])

    def p_expression_not(self, p):
        "expression : NOT expression"
        p[0] = Not(p[2])

    def p_expression_paren(self, p):
        "expression : LPAREN expression RPAREN"
        p[0] = p[2]

    def p_expression_eq(self, p):
        "expression : expression EQ expression"
        p[0] = Equal(p[1], p[3])

    def p_expression_ne(self, p):
        "expression : expression NE expression"
        p[0] = Not(Equal(p[1], p[3]))

    def p_expression_gt(self, p):
        "expression : expression GT expression"
        p[0] = Greater(p[1], p[3])

    def p_expression_gte(self, p):
        "expression : expression GTE expression"
        p[0] = GreaterEq(p[1], p[3])

    def p_expression_lt(self, p):
        "expression : expression LT expression"
        p[0] = Less(p[1], p[3])

    def p_expression_lte(self, p):
        "expression : expression LTE expression"
        p[0] = LessEq(p[1], p[3])

    def p_expression_match(self, p):
        "expression : expression TILDE expression"
        p[0] = Match(p[1], p[3])

    def p_expression_column(self, p):
        "expression : column"
        p[0] = p[1]

    def p_expression_constant(self, p):
        "expression : constant"
        p[0] = p[1]

    def p_expression_function(self, p):
        "expression : ID LPAREN expression_list RPAREN"
        p[0] = Function(p[1], p[3])

    def p_expression_list(self, p):
        """
        expression_list : empty
                        | expression
                        | expression_list COMMA expression
        """
        p[0] = self.handle_comma_separated_list(p)

    def p_column(self, p):
        """
        column : ID
        """
        p[0] = Column(p[1])

    def p_column_list(self, p):
        """
        column_list : column
                    | column_list COMMA column
        """
        p[0] = self.handle_comma_separated_list(p)

    def p_constant(self, p):
        """
        constant : NULL
                 | boolean
                 | INTEGER
                 | DECIMAL
                 | STRING
                 | DATE
        """
        p[0] = Constant(p[1] if p[1] != 'NULL' else None)

    def p_boolean(self, p):
        """
        boolean : TRUE
                | FALSE
        """
        p[0] = (p[1] == 'TRUE')

    def p_empty(self, _):
        """
        empty :
        """

    def p_error(self, token):
        if token is None:
            raise ParseError("ERROR: unterminated statement. Missing a semicolon?")
        else:
            oss = io.StringIO()
            oss.write("ERROR: Syntax error near '{}' (at {})\n".format(token.value,
                                                                       token.lexpos))
            oss.write("  ")
            oss.write(self._input)
            oss.write("\n")
            oss.write("  {}^".format(' ' * token.lexpos))
            raise ParseError(oss.getvalue())



# FIXME:
# - Create a RowContext object that provides all the rows, so that we can
# - Add year month date as columns, but should not be included in * default
# - Add date() function to create dates from a string
# - Check data types for functions
# - Actually allow evaluating the SQL against generic rows of datasets.
# - Implement aggregation
# - Render with custom routine, not beancount.reports.table
# - Deal with rendering on multiple lines, e.g., for inventories with multiple positions
# - Make it possible to run from the command-line (batch)
# - Invoke a pager when long output
# - Implement set variables for format and verbosity and display precision and what-not
# - Implement JOURNAL account FROM
# - Implement BALANCES FROM
# - pipe through a pager
# - Find a way to pipe into treeify
# - Find a way to trigger a close in the FROM clause
# - implement order by
# - implement limit
# - implement distinct
# - support simple boolean expressions in filter expressions, not just equalities and inequalities
# - support simple mathematical operations, +, - , /.
# - implement set operations, "in" for sets
# - implement globbing matches
# - case-sensitivity of regexps?
# - make KeyboardInterrupt not exit the shell, just interrupt the current processing of a query.
