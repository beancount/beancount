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


# A 'select' query action.
Select = collections.namedtuple(
    'Select', ('targets from_clause where_clause '
               'group_by order_by pivot_by limit flatten'))

# A wildcard node, to appear in Select.columns.
Wildcard = collections.namedtuple('Wildcard', '')

# A node for ordering.
FromFilter = collections.namedtuple('From', 'expression close')
GroupBy = collections.namedtuple('GroupBy', 'columns having')
OrderBy = collections.namedtuple('OrderBy', 'columns ordering')
PivotBy = collections.namedtuple('PivotBy', 'columns')


class Comparable:
    __slots__ = ()

    def __eq__(self, other):
        if not isinstance(other, type(self)):
            return False
        return all(getattr(self, attribute) == getattr(other, attribute)
                   for attribute in self.__slots__)

    def __str__(self):
        return "{}({})".format(type(self).__name__,
                               ', '.join(repr(getattr(self, child))
                                         for child in self.__slots__))
    __repr__ = __str__

    def reset(self):
        """Reset the state of the aggregator functions."""
        raise NotImplementedError


class Target(Comparable):
    __slots__ = ('expression', 'name')
    def __init__(self, expression, name=None):
        self.expression = expression
        self.name = name

    def __call__(self, context):
        return self.expression(context)

    def reset(self):
        self.expression.reset()


class Column(Comparable):
    __slots__ = ('name',)
    def __init__(self, name):
        self.name = name

class Function(Comparable):
    __slots__ = ('fname', 'operands')
    def __init__(self, fname, operands):
        self.fname = fname
        self.operands = operands or []

class UnaryOp(Comparable):
    __slots__ = ('operand',)
    def __init__(self, operand):
        self.operand = operand

    def reset(self):
        self.operand.reset()


class BinaryOp(Comparable):
    __slots__ = ('left', 'right')
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def reset(self):
        self.left.reset()
        self.right.reset()


class Constant(Comparable):
    __slots__ = ('constant',)
    def __init__(self, constant):
        self.constant = constant
    def __call__(self, _):
        return self.operand

class Not(UnaryOp):
    def __call__(self, context):
        return not self.operand(context)

class Equal(BinaryOp):
    def __call__(self, context):
        return self.left(context) == self.right(context)

class ReMatch(BinaryOp):
    def __call__(self, context):
        return re.match(self.right(context), self.left(context))

class And(BinaryOp):
    def __call__(self, context):
        return (self.left(context) and self.right(context))

class Or(BinaryOp):
    def __call__(self, context):
        return (self.left(context) or self.right(context))


class ParseError(Exception):
    """A parser error."""


class Lexer:
    """PLY lexer for the Beancount Query Language.
    """

    # List of reserved keywords.
    keywords = {
        'SELECT', 'AS', 'FROM', 'WHERE', 'CLOSE', 'ON',
        'GROUP', 'BY', 'HAVING', 'ORDER', 'DESC', 'ASC', 'PIVOT',
        'LIMIT', 'FLATTEN',
        'AND', 'OR', 'NOT', 'TRUE', 'FALSE',
        'NULL',
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
        """
        p[0] = p[1]

    def p_select_statement(self, p):
        """
        select_statement : SELECT target_spec from where \
                           group_by order_by pivot_by limit flatten
        """
        p[0] = Select(p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9])

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

    def p_from(self, p):
        """
        from : empty
             | FROM expression close
             | FROM LPAREN select_statement RPAREN
        """
        if len(p) == 4:
            p[0] = FromFilter(p[2], p[3])
        elif len(p) == 5:
            p[0] = p[3]

    def p_close(self, p):
        """
        close : empty
              | CLOSE
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
        ('left', 'EQ', 'NE', 'TILDE'),
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

    def p_expression_match(self, p):
        "expression : expression TILDE expression"
        p[0] = ReMatch(p[1], p[3])

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
# - Add date data type as literal
# - Create a RowContext object that provides all the rows, so that we can
# - Make column a list of the same type as the postings filter expression
# Compilation:
# - Identify functions
# - Add date() function to create dates from a string
# - Check data types
#   actually evaluate the SQL against generic rows of datasets.
# - Move all the functions to another module, make this as generic SQL as can possibly be
# - begin implementing unit tests
# - tokenize and parse all data types, including dates
# - differentiate clearly between entry and posting columns
# - differentiate clearly between entry and posting functions
# - implement inequalities and regexp matching
# - implement aggregation
# - render with custom routine, not beancount.reports.table
# - deal with rendering on multiple lines, e.g., for inventories with multiple positions
# - make it possible to run from the command-line (batch)
# - check data types for functions
# - invoke a pager when long output
# - implement set variables for format and verbosity and display precision and what-not
# - implement JOURNAL account FROM
# - implement BALANCES FROM
# - find a way to pipe into treeify
# - find a way to trigger a close in the FROM clause
# - implement order by
# - implement limit
# - implement distinct
# - support simple boolean expressions in filter expressions, not just equalities and inequalities
# - support simple mathematical operations, +, - , /.
# - implement set operations, "in" for sets
