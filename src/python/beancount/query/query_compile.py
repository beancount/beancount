"""Compilation/translation for the query language AST.

This code accepts the abstract syntax tree produced by the query parser and
resolves the column and function names, and prepares execution state to be run
against a list of entries.
"""
import collections
import copy
import itertools
import io
import re

import ply.lex
import ply.yacc

from beancount.core import position
from beancount.core import data
from beancount.query import query_parser



class CompilationError(Exception):
    """A compiler/interpreter error."""



class EvalNode:
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


class EvalUnaryOp(EvalNode):
    __slots__ = ('operand',)

    def __init__(self, operand):
        self.operand = operand

    def reset(self):
        self.operand.reset()

class EvalBinaryOp(EvalNode):
    __slots__ = ('left', 'right')

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def reset(self):
        self.left.reset()
        self.right.reset()

class EvalConstant(EvalNode):
    __slots__ = ('value',)

    def __init__(self, value):
        self.value = value

    def __call__(self, _):
        return self.value

class EvalNot(EvalUnaryOp):

    def __call__(self, context):
        return not self.operand(context)

class EvalEqual(EvalBinaryOp):

    def __call__(self, context):
        return self.left(context) == self.right(context)

class EvalMatch(EvalBinaryOp):

    def __call__(self, context):
        return re.search(self.right(context), self.left(context))

class EvalAnd(EvalBinaryOp):

    def __call__(self, context):
        return (self.left(context) and self.right(context))

class EvalOr(EvalBinaryOp):

    def __call__(self, context):
        return (self.left(context) or self.right(context))

class EvalFunction(EvalNode):
    __slots__ = ('operands',)
    def __init__(self, operands):
        self.operands = operands or []

    def reset(self):
        for operand in self.operands:
            operand.reset()

    def eval_args(self, context):
        return [operand(context)
                for operand in self.operands]

# Interpreter nodes.
OPERATORS = {
    query_parser.Constant: EvalConstant,
    query_parser.Not: EvalNot,
    query_parser.Equal: EvalEqual,
    query_parser.Match: EvalMatch,
    query_parser.And: EvalAnd,
    query_parser.Or: EvalOr,
    }



# Simple (i.e., not aggregating) function objects. These functionals maintain no
# state.

class Length(EvalFunction):
    def __call__(self, posting):
        args = self.eval_args(posting)
        return len(args[0])

class Year(EvalFunction):
    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].year

class Month(EvalFunction):
    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].month

class Day(EvalFunction):
    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].day

class Units(EvalFunction):
    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].get_amount()

class Cost(EvalFunction):
    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].get_cost()

SIMPLE_FUNCTIONS = {
    'length': Length,
    'units': Units,
    'cost': Cost,
    'year': Year,
    'month': Month,
    'day': Day,
    }



# Aggregating functions. These instances themselves both make the computation
# and manage state for a single iteration.

class Aggregator(EvalFunction):
    def __init__(self, operands):
        super().__init__(operands)
        self.reset()

    def reset(self):
        self.state = None

class Sum(Aggregator):
    def __call__(self, posting):
        args = self.eval_args(posting)
        value = args[0]
        if self.state is None:
            self.state = value
        else:
            self.state += value

class First(Aggregator):
    def __call__(self, posting):
        if self.state is None:
            args = self.eval_args(posting)
            self.state = args[0]

class Last(Aggregator):
    def __call__(self, posting):
        args = self.eval_args(posting)
        self.state = args[0]

AGGREGATOR_FUNCTIONS = {
    'sum': Sum,
    'first': First,
    'last': Last,
    }



class CompilationContext:
    """Base class for all compilation contexts. A compilation context provides
    column accessors specific to the particular row objects that we will access.
    """
    # The name of the context.
    context_name = None

    # Maps of names to evaluators for columns and functions.
    columns = None
    functions = None

    def get_column(self, name):
        """Return a column accessor for the given named column.
        Args:
          name: A string, the name of the column to access.
        """
        try:
            return self.columns[name]()
        except KeyError:
            raise CompilationError("Invalid column name '{}' in {} context.".format(
                name, self.context_name))

    def get_function(self, name, operands):
        """Return a function accessor for the given named function.
        Args:
          name: A string, the name of the function to access.
        """
        try:
            return self.functions[name](operands)
        except KeyError:
            raise CompilationError("Invalid function name '{}' in {} context.".format(
                name, self.context_name))



# Column accessors for entries.

class TypeEntryColumn(EvalNode):
    def __call__(self, entry):
        return type(entry).__name__

class FilenameEntryColumn(EvalNode):
    def __call__(self, entry):
        return entry.source.filename

class LineNoEntryColumn(EvalNode):
    def __call__(self, entry):
        return entry.source.lineno

class DateEntryColumn(EvalNode):
    def __call__(self, entry):
        return entry.date

class FlagEntryColumn(EvalNode):
    def __call__(self, entry):
        return entry.flag

class PayeeEntryColumn(EvalNode):
    def __call__(self, entry):
        return entry.payee or ''

class NarrationEntryColumn(EvalNode):
    def __call__(self, entry):
        return entry.narration

class TagsEntryColumn(EvalNode):
    def __call__(self, entry):
        return entry.tags

class LinksEntryColumn(EvalNode):
    def __call__(self, entry):
        return entry.links

class FilterEntriesContext(CompilationContext):
    """An execution context that provides access to attributes on Transactions
    and other entry types.
    """
    context_name = 'FROM clause'
    columns = {
        'type' : TypeEntryColumn,
        'filename' : FilenameEntryColumn,
        'lineno' : LineNoEntryColumn,
        'date' : DateEntryColumn,
        'flag' : FlagEntryColumn,
        'payee' : PayeeEntryColumn,
        'narration' : NarrationEntryColumn,
        'tags' : TagsEntryColumn,
        'links' : LinksEntryColumn,
        }
    functions = SIMPLE_FUNCTIONS



# Column accessors for postings.

class TypeColumn(EvalNode):
    def __call__(self, posting):
        return type(posting.entry).__name__

class FilenameColumn(EvalNode):
    def __call__(self, posting):
        return posting.entry.source.filename

class LineNoColumn(EvalNode):
    def __call__(self, posting):
        return posting.entry.source.lineno

class DateColumn(EvalNode):
    def __call__(self, posting):
        return posting.entry.date

class FlagColumn(EvalNode):
    def __call__(self, posting):
        return posting.entry.flag

class PayeeColumn(EvalNode):
    def __call__(self, posting):
        return posting.entry.payee or ''

class NarrationColumn(EvalNode):
    def __call__(self, posting):
        return posting.entry.narration

class TagsColumn(EvalNode):
    def __call__(self, posting):
        return posting.entry.tags

class LinksColumn(EvalNode):
    def __call__(self, posting):
        return posting.entry.links

class AccountColumn(EvalNode):
    def __call__(self, posting):
        return posting.account

class NumberColumn(EvalNode):
    def __call__(self, posting):
        return posting.position.number

class CurrencyColumn(EvalNode):
    def __call__(self, posting):
        return posting.position.lot.currency

class ChangeColumn(EvalNode):
    def __call__(self, posting):
        return posting.position

class FilterPostingsContext(CompilationContext):
    """An execution context that provides access to attributes on Postings.
    """
    context_name = 'WHERE clause'
    columns = {
        'type'      : TypeColumn,
        'filename'  : FilenameColumn,
        'lineno'    : LineNoColumn,
        'date'      : DateColumn,
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
    functions = SIMPLE_FUNCTIONS

class TargetsContext(FilterPostingsContext):
    """An execution context that provides access to attributes on Postings.
    """
    context_name = 'targets/column'
    functions = copy.copy(FilterPostingsContext.functions)
    functions.update(AGGREGATOR_FUNCTIONS)



class AttributeColumn(query_parser.Column):
    def __call__(self, row):
        return getattr(row, self.name)

class ResultSetContext(CompilationContext):
    """An execution context that provides access to attributes from a result set.
    """
    context_name = 'sub-query'

    def get_column(self, name):
        """Override the column getter to provide a single attribute getter.
        """
        return AttributeColumn(name)




def compile_expression(expr, xcontext):
    """Bind an expression to its execution context.

    Args:
      expr: The root node of an expression.
      xcontext: An CompilationContext instance.
    Returns:
      The root node of a bound expression.
    """
    # Convert column references to the context.
    if isinstance(expr, query_parser.Column):
        c_expr = xcontext.get_column(expr.name)

    elif isinstance(expr, query_parser.Function):
        c_operands = [compile_expression(operand, xcontext)
                      for operand in expr.operands]
        c_expr = xcontext.get_function(expr.fname, c_operands)

    elif isinstance(expr, query_parser.UnaryOp):
        eval_type = OPERATORS[type(expr)]
        c_expr = eval_type(compile_expression(expr.operand, xcontext))

    elif isinstance(expr, query_parser.BinaryOp):
        eval_type = OPERATORS[type(expr)]
        c_expr = eval_type(compile_expression(expr.left, xcontext),
                           compile_expression(expr.right, xcontext))

    elif isinstance(expr, query_parser.Constant):
        c_expr = EvalConstant(expr.value)

    else:
        assert False, "Invalid expression to compile: {}".format(expr)

    # Check data types.
    # FIXME: TODO

    # Check arity of functions.
    # FIXME: TODO

    # Establish whether this is an operation that operates on aggregates.

    return c_expr



def compile_select(select):
    """Prepare an AST for a Select statement into a very rudimentary execution tree.
    The execution tree mostly looks much like an AST, but with some nodes
    replaced with knowledge specific to an execution context and eventually some
    basic optimizations.

    Args:
      select: An instance of query_parser.Select.
    Returns:
      A modified instance of Select with some nodes replaced.
    """

    # Process the FROM clause and figure out the execution context for the
    # targets and the where clause.
    from_clause = select.from_clause
    if isinstance(from_clause, query_parser.Select):
        c_from = compile_select(from_clause) if from_clause is not None else None
        xcontext_target = ResultSetContext()
        xcontext_where = ResultSetContext()

    elif from_clause is None or isinstance(from_clause, query_parser.From):
        # Bind the from clause contents.
        xcontext_entries = FilterEntriesContext()
        if from_clause is not None:
            c_expression = (compile_expression(from_clause.expression, xcontext_entries)
                            if from_clause.expression is not None
                            else None)
            c_from = query_parser.From(c_expression, from_clause.close)
        else:
            c_from = None
        xcontext_target = TargetsContext()
        xcontext_where = FilterPostingsContext()

    else:
        raise CompilationError("Unexpected from clause in AST: {}".format(from_clause))

    # Bind the targets expressions to the execution context.
    targets = select.targets
    if isinstance(targets, query_parser.Wildcard):
        # Insert the full list of available columns.
        targets = [query_parser.Target(query_parser.Column(name), None)
                   for name in TargetsContext.columns]

    # Compile targets.
    c_targets = []
    for target in targets:
        c_expr = compile_expression(target.expression, xcontext_target)
        # FIXME: Find a better way to come up with a decent name.
        target_name = target.name
        if target_name is None:
            sub_name = re.sub('[^a-z]+', '_',str(target.expression).lower())
            target_name = re.sub('column', '', sub_name).strip('_')
        c_targets.append(query_parser.Target(c_expr, target_name))

    # Bind the WHERE expression to the execution context.
    # Note: Aggregates are disallowed in this clause.
    # FIXME: check for this!
    c_where = (compile_expression(select.where_clause, xcontext_where)
               if select.where_clause is not None
               else None)

    # Is this an aggregated query or not?



    # Check that the group-by column references are valid w.r.t. aggregates.
    # Check that the pivot-by column references are valid.
    # Check that the order-by column references are valid.



    # print()
    # print()
    # print(select)
    # print()

    return query_parser.Select(c_targets,
                               c_from,
                               c_where,
                               select.group_by,
                               select.order_by,
                               select.pivot_by,
                               select.limit,
                               select.distinct,
                               select.flatten)


def interpret_select(entries, c_select):
    """Given a compiled select statement, execute the query.

    Args:
      entries: A list of directives.
      c_select: An instance of a compiled Select query, from the parser.
    """
    # Create a class for the row.
    Tuple = collections.namedtuple('Tuple',
                                   [target.name for target in c_select.targets])

    # Filter the entries.
    if c_select.from_clause:
        expression = c_select.from_clause.expression
        filtered_entries = []
        for entry in entries:
            if isinstance(entry, data.Transaction):
                if expression(entry):
                    filtered_entries.append(entry)
            else:
                filtered_entries.append(entry)
    else:
        filtered_entries = entries

    # Process all the postings.
    rows = []
    expression = c_select.where_clause
    try:
        for entry in filtered_entries:
            if isinstance(entry, data.Transaction):
                for posting in entry.postings:
                    if expression is None or expression(posting):
                        row = Tuple(*[target.expression(posting)
                                      for target in c_select.targets])
                        rows.append(row)
                        if c_select.limit and len(rows) == c_select.limit:
                            raise StopIteration
    except StopIteration:
        pass

    # Flatten if required.


    return rows
