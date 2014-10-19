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

from beancount.query.query_parser import Comparable
from beancount.core import position
from beancount.core import data
from beancount.query import query_parser



class CompilationError(Exception):
    """A compiler/interpreter error."""



class EvalFunction(Comparable):
    __slots__ = ('operands',)
    def __init__(self, operands):
        self.operands = operands or []

    def reset(self):
        for operand in self.operands:
            operand.reset()

    def eval_args(self, context):
        return [operand(context)
                for operand in self.operands]



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
        child_value = self.child(posting)
        if self.state is None:
            self.state = child_value
        else:
            self.state += child_value

class First(Aggregator):
    def __call__(self, posting):
        if self.state is None:
            self.state = self.child(posting)

class Last(Aggregator):
    def __call__(self, posting):
        self.state = self.child(posting)

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

class TypeEntryColumn(Comparable):
    def __call__(self, entry):
        return type(entry).__name__

class FilenameEntryColumn(Comparable):
    def __call__(self, entry):
        return entry.source.filename

class LineNoEntryColumn(Comparable):
    def __call__(self, entry):
        return entry.source.lineno

class DateEntryColumn(Comparable):
    def __call__(self, entry):
        return entry.date

class YearEntryColumn(Comparable):
    def __call__(self, entry):
        return entry.date.year

class FlagEntryColumn(Comparable):
    def __call__(self, entry):
        return entry.flag

class PayeeEntryColumn(Comparable):
    def __call__(self, entry):
        return entry.payee

class NarrationEntryColumn(Comparable):
    def __call__(self, entry):
        return entry.narration

class TagsEntryColumn(Comparable):
    def __call__(self, entry):
        return entry.tags

class LinksEntryColumn(Comparable):
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
        'year' : YearEntryColumn,
        'flag' : FlagEntryColumn,
        'payee' : PayeeEntryColumn,
        'narration' : NarrationEntryColumn,
        'tags' : TagsEntryColumn,
        'links' : LinksEntryColumn,
        }
    functions = SIMPLE_FUNCTIONS



# Column accessors for postings.

class TypeColumn(Comparable):
    def __call__(self, posting):
        return type(posting.entry).__name__

class FilenameColumn(Comparable):
    def __call__(self, posting):
        return posting.entry.source.filename

class LineNoColumn(Comparable):
    def __call__(self, posting):
        return posting.entry.source.lineno

class DateColumn(Comparable):
    def __call__(self, posting):
        return posting.entry.date

class YearColumn(Comparable):
    def __call__(self, posting):
        return posting.entry.date.year

class FlagColumn(Comparable):
    def __call__(self, posting):
        return posting.entry.flag

class PayeeColumn(Comparable):
    def __call__(self, posting):
        return posting.entry.payee

class NarrationColumn(Comparable):
    def __call__(self, posting):
        return posting.entry.narration

class TagsColumn(Comparable):
    def __call__(self, posting):
        return posting.entry.tags

class LinksColumn(Comparable):
    def __call__(self, posting):
        return posting.entry.links

class AccountColumn(Comparable):
    def __call__(self, posting):
        return posting.account

class NumberColumn(Comparable):
    def __call__(self, posting):
        return posting.position.number

class CurrencyColumn(Comparable):
    def __call__(self, posting):
        return posting.position.lot.currency

class ChangeColumn(Comparable):
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
    functions = SIMPLE_FUNCTIONS

class TargetsContext(FilterPostingsContext):
    """An execution context that provides access to attributes on Postings.
    """
    context_name = 'targets/column'
    functions = copy.copy(FilterPostingsContext.columns)
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
        c_expr = type(expr)(compile_expression(expr.operand, xcontext))

    elif isinstance(expr, query_parser.BinaryOp):
        c_expr = type(expr)(compile_expression(expr.left, xcontext),
                            compile_expression(expr.right, xcontext))

    elif isinstance(expr, query_parser.Constant):
        c_expr = expr

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

    elif from_clause is None or isinstance(from_clause, query_parser.FromFilter):
        # Bind the from clause contents.
        xcontext_entries = FilterEntriesContext()
        if from_clause is not None:
            c_expression = (compile_expression(from_clause.expression, xcontext_entries)
                            if from_clause.expression is not None
                            else None)
            c_from = query_parser.FromFilter(c_expression, from_clause.close)
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
        target_name = target.name or re.sub('[^a-z]+', '_',
                                            str(target.expression).lower()).strip('_')
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
    for entry in filtered_entries:
        if isinstance(entry, data.Transaction):
            for posting in entry.postings:
                if expression is None or expression(posting):
                    row = Tuple(*[target(posting) for target in c_select.targets])
                    rows.append(row)

    # Flatten if required.


    return rows
