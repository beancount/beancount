"""Parser for Beancount Query Language.
"""
import collections
import itertools
import io

import ply.lex
import ply.yacc

from beancount.core import position


class Comparable:
    __slots__ = ()
    def __eq__(self, other):
        if not isinstance(other, type(self)):
            return False
        return all(getattr(self, attribute) == getattr(other, attribute)
                   for attribute in self.__slots__)

class Column(Comparable):
    def __init__(self, name):
        self.name = name





class TypeColumn(Column):
    def __call__(self, posting):
        return type(posting.entry)

class FilenameColumn(Column):
    def __call__(self, posting):
        return posting.entry.source.filename

class LineNoColumn(Column):
    def __call__(self, posting):
        return posting.entry.source.lineno

class DateColumn(Column):
    def __call__(self, posting):
        return posting.entry.date

class YearColumn(Column):
    def __call__(self, posting):
        return posting.entry.date.year

class FlagColumn(Column):
    def __call__(self, posting):
        return posting.entry.flag

class PayeeColumn(Column):
    def __call__(self, posting):
        return posting.entry.payee

class NarrationColumn(Column):
    def __call__(self, posting):
        return posting.entry.narration

class TagsColumn(Column):
    def __call__(self, posting):
        return posting.entry.tags

class LinksColumn(Column):
    def __call__(self, posting):
        return posting.entry.links

class AccountColumn(Column):
    def __call__(self, posting):
        return posting.account

class NumberColumn(Column):
    def __call__(self, posting):
        return posting.position.number

class CurrencyColumn(Column):
    def __call__(self, posting):
        return posting.position.lot.currency

class ChangeColumn(Column):
    def __call__(self, posting):
        return posting.position


POSTING_COLUMNS = {
    'type'      : TypeColumn,
    'filename'  : FilenameColumn,
    'lineno'    : LineNoColumn,
    'date'      : DateColumn,
    'year'      : YearColumn,
    'flag'      : FlagColumn,
    'payee'     : PayeeColumn,
    'narration' : NarrationColumn,
    'tags'      : TagsColumn,
    'links'     : LinksColumn,
    'account'   : AccountColumn,
    'number'    : NumberColumn,
    'currency'  : CurrencyColumn,
    'change'    : ChangeColumn,
    }



class Function(Column):
    def __init__(self, child, **kwds):
        super().__init__(**kwds)
        self.child = child

class Length(Function):
    def __call__(self, posting):
        child_value = self.child(posting)
        return len(child_value)

class Units(Function):
    def __call__(self, posting):
        position_ = self.child(posting)
        assert isinstance(position_, position.Position)
        return position_.get_amount()

class Cost(Function):
    def __call__(self, posting):
        position_ = self.child(posting)
        assert isinstance(position_, position.Position)
        return position_.get_cost()


class Aggregator(Function):
    def __init__(self, **kwds):
        super().__init__(**kwds)
        self.agg_value = None

class Sum(Aggregator):
    def __call__(self, posting):
        child_value = self.child(posting)
        if self.agg_value is None:
            self.agg_value = child_value
        else:
            self.agg_value += child_value

class First(Aggregator):
    def __call__(self, posting):
        if self.agg_value is None:
            self.agg_value = self.child(posting)

class Last(Aggregator):
    def __call__(self, posting):
        self.agg_value = self.child(posting)


FUNCTIONS = {
    'length': Length,
    'units': Units,
    'cost': Cost,
    'sum': Sum,
    'first': First,
    'last': Last,
    }



class DateEntryColumn(Column):
    def __call__(self, entry):
        return entry.date

class FlagEntryColumn(Column):
    def __call__(self, entry):
        return entry.flag

# FIXME: These ought to be functions, as in YEAR(date) or MONTH(date), that work
# on both types of entries (they would work on dates, really).
class YearEntryColumn(Column):
    def __call__(self, entry):
        return entry.date.year

class MonthEntryColumn(Column):
    def __call__(self, entry):
        return entry.date.month

ENTRY_COLUMNS = {
    'date'  : DateEntryColumn,
    'flag'  : FlagEntryColumn,
    'year'  : YearEntryColumn,
    'month' : MonthEntryColumn,
    }



Query = collections.namedtuple('Query',
                               'columns entry_filter posting_filter')


class Expr(Comparable):
    def __call__(self, context):
        raise NotImplementedError

class UnaryOp(Expr):
    __slots__ = ('operand',)
    def __init__(self, operand):
        self.operand = operand

class BinaryOp(Expr):
    __slots__ = ('left', 'right')
    def __init__(self, left, right):
        self.left = left
        self.right = right

class Constant(UnaryOp):
    def __call__(self, context):
        return self.operand

class Not(UnaryOp):
    def __call__(self, context):
        return not self.operand(context)

class Eq(BinaryOp):
    def __call__(self, context):
        return self.left(context) == self.right(context)

class Ne(BinaryOp):
    def __call__(self, context):
        return self.left(context) != self.right(context)

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
        'SELECT', 'FROM', 'WHERE',
        # 'GROUP', 'ORDER', 'BY', 'LIMIT', 'DESC', 'ASC',
        'AND', 'OR', 'NOT',
        'NULL',
    }

    # List of valid tokens from the lexer.
    tokens = [
        'ID', 'INTEGER', 'STRING',
        'COMMA', 'SEMI', 'LPAREN', 'RPAREN', 'EQ', 'NE'
    ] + list(keywords)

    # An identifier, for a target or a dimension or whatever.
    def t_ID(self, token):
        "[a-zA-Z][a-zA-Z_]*"
        utoken = token.value.upper()
        if utoken in self.keywords:
            token.type = utoken
        token.value = token.value.lower()
        return token

    def t_STRING(self, token):
        "(\"[^\"]*\"|\'[^\']*\')"
        token.value = token.value[1:-1]
        return token

    # Constant tokens.
    t_COMMA  = r","
    t_SEMI   = r";"
    t_LPAREN = r"\("
    t_RPAREN = r"\)"
    t_NE     = r"!="
    t_EQ     = r"="

    # Numbers.
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
    start = 'select_statement'

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

    def p_select_statement(self, p):
        """
        select_statement : SELECT target_list opt_from opt_where SEMI
        """
        p[0] = Query(p[2],
                     p[3] or Constant(True),
                     p[4] or Constant(True))

    def p_target_list(self, p):
        """
        target_list : target
                    | target_list COMMA target
        """
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1]
            p[0].append(p[3])

    def p_target(self, p):
        """
        target : funcall
               | ID
        """
        if isinstance(p[1], str):
            identifier = p[1]
            try:
                cls = POSTING_COLUMNS[identifier]
                p[0] = cls(name=identifier)
            except KeyError:
                raise ParseError("Invalid column name: '{}'".format(identifier))
        else:
            p[0] = p[1]

    def p_function(self, p):
        """
        funcall : ID LPAREN target RPAREN
        """
        function_name = p[1]
        try:
            FunctionType = FUNCTIONS[function_name]
            p[0] = FunctionType(child=p[3], name=function_name)
        except KeyError:
            raise ParseError("Invalid function name: '{}'".format(function_name))

    # FIXME: Differentiate between entry expresssions and posting expressions
    def p_opt_from(self, p):
        """
        opt_from : empty
                 | FROM expression
        """
        if len(p) == 3:
            assert p[2], "Empty FROM clause is not allowed"
            p[0] = p[2]

    def p_opt_where(self, p):
        """
        opt_where : empty
                  | WHERE expression
        """
        if len(p) == 3:
            assert p[2], "Empty WHERE clause is not allowed"
            p[0] = p[2]

    precedence = [
        ('left', 'EQ', 'NE'),
        ('left', 'OR'),
        ('left', 'AND'),
        ('left', 'NOT'),
        ]

    def p_expression(self, p):
        """
        expression : expression AND expression
                   | expression OR expression
                   | NOT expression
                   | LPAREN expression RPAREN
                   | predicate
        """
        if len(p) == 2:
            # predicate
            p[0] = p[1]
        elif len(p) == 3:
            # NOT expression
            p[0] = Not(p[2])
        elif p[1] == '(':
            p[0] = p[2]
        elif p[2].upper() == 'AND':
            p[0] = And(p[1], p[3])
        elif p[2].upper() == 'OR':
            p[0] = Or(p[1], p[3])
        else:
            assert False, "Internal error"

    def p_predicate(self, p):
        """
        predicate : entry_target EQ value
                  | entry_target NE value
        """
        if p[2] == '=':
            p[0] = Eq(p[1], Constant(p[3]))
        elif p[2] == '!=':
            p[0] = Ne(p[1], Constant(p[3]))
        else:
            assert False, "Internal error"

    def p_value(self, p):
        """
        value : NULL
              | INTEGER
              | STRING
        """
        if p[1] == 'NULL':
            p[0] = None
        else:
            p[0] = p[1]

    def p_entry_target(self, p):
        """
        entry_target : ID
        """
        identifier = p[1]
        try:
            cls = ENTRY_COLUMNS[identifier]
            p[0] = cls(name=identifier)
        except KeyError:
            raise ParseError("Invalid entry column name: '{}'".format(identifier))

    def p_empty(self, _):
        """
        empty :
        """
    def p_error(self, token):
        if token is None:
            raise ParseError("ERROR: unterminated statement")
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
# - Create a constant holder object and instantiate it
# - Test the AST tree separately, for comparisons and such
# - Create a RowContext object that provides all the rows, so that we can
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
