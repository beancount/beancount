"""Interpreter for the query language's AST.

This code accepts the abstract syntax tree produced by the query parser,
resolves the column and function names, compiles and interpreter and prepares a
query to be run against a list of entries.
"""
import collections
import copy
import datetime
import itertools
import io
import re

import ply.lex
import ply.yacc

from beancount.core.amount import Decimal
from beancount.core import position
from beancount.core import inventory
from beancount.core import data
from beancount.query import query_parser



class CompilationError(Exception):
    """A compiler/interpreter error."""



class EvalNode:
    __slots__ = ('dtype',)

    def __init__(self, dtype):
        # The output data type produce by this node. This is intended to be
        # inferred by the nodes on construction.
        assert dtype is not None, "Internal erro: Invalid dtype, must be deduced."
        self.dtype = dtype

    def __eq__(self, other):
        """Override the equality operator to compare the data type and a all attributes
        of this node. This is used by tests for comparing nodes.
        """
        return (isinstance(other, type(self))
                and all(
                    getattr(self, attribute) == getattr(other, attribute)
                    for attribute in self.__slots__))

    def __str__(self):
        return "{}({})".format(type(self).__name__,
                               ', '.join(repr(getattr(self, child))
                                         for child in self.__slots__))
    __repr__ = __str__

    def childnodes(self):
        """Returns the child nodes of this node.
        Yields:
          A list of EvalNode instances.
        """
        for attr in self.__slots__:
            child = getattr(self, attr)
            if isinstance(child, EvalNode):
                yield child
            elif isinstance(child, list):
                for element in child:
                    if isinstance(element, EvalNode):
                        yield element

    ## FIXME: remove
    # def visit(self, visitor):
    #     """In-order visitor.
    #     Args:
    #       visitor: A callable that will get invoked with this instance.
    #     """
    #     try:
    #         visitor(self)
    #         for attr in self.__slots__:
    #             child = getattr(self, attr)
    #             if isinstance(child, EvalNode):
    #                 child.visit(visitor)
    #     except StopIteration:
    #         pass



class EvalUnaryOp(EvalNode):
    __slots__ = ('operand',)

    def __init__(self, operand, dtype):
        super().__init__(dtype)
        self.operand = operand

class EvalBinaryOp(EvalNode):
    __slots__ = ('left', 'right')

    def __init__(self, left, right, dtype):
        super().__init__(dtype)
        self.left = left
        self.right = right

class EvalConstant(EvalNode):
    __slots__ = ('value',)

    def __init__(self, value):
        super().__init__(type(value))
        self.value = value

    def __call__(self, _):
        return self.value

class EvalNot(EvalUnaryOp):

    def __init__(self, operand):
        super().__init__(operand, bool)

    def __call__(self, context):
        return not self.operand(context)

class EvalEqual(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(left, right, bool)

    def __call__(self, context):
        return self.left(context) == self.right(context)

class EvalMatch(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(left, right, bool)
        if right.dtype != str:
            raise CompilationError(
                "Invalid data type for RHS of match: '{}'; must be a string".format(
                    right.dtype))

    def __call__(self, context):
        return re.search(self.right(context), self.left(context))

class EvalAnd(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(left, right, bool)

    def __call__(self, context):
        return (self.left(context) and self.right(context))

class EvalOr(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(left, right, bool)

    def __call__(self, context):
        return (self.left(context) or self.right(context))

# Interpreter nodes.
OPERATORS = {
    query_parser.Constant: EvalConstant,
    query_parser.Not: EvalNot,
    query_parser.Equal: EvalEqual,
    query_parser.Match: EvalMatch,
    query_parser.And: EvalAnd,
    query_parser.Or: EvalOr,
    }



ANY = object()

class EvalFunction(EvalNode):
    """Base class for all function objects."""
    __slots__ = ('operands',)

    # Type constraints on the input arguments.
    __intypes__ = []

    def __init__(self, operands, dtype):
        super().__init__(dtype)
        assert isinstance(operands, list), "Internal error: invalid type for operands."
        self.operands = operands

        # Check the data types
        if len(operands) != len(self.__intypes__):
            raise CompilationError(
                "Invalid number of arguments for {}: found {} expected {}".format(
                    type(self).__name__, len(operands), len(self.__intypes__)))

        # Check each of the types.
        for index, (operand, intype) in enumerate(zip(operands, self.__intypes__)):
            if not issubclass(operand.dtype, intype):
                raise CompilationError(
                    "Invalid type for argument {} of {}: found {} expected {}".format(
                        index, type(self).__name__, operand.dtype, intype))

    def eval_args(self, context):
        return [operand(context)
                for operand in self.operands]


# Simple (i.e., not aggregating) function objects. These functionals maintain no
# state.

class Length(EvalFunction):
    __intypes__ = [(list, set, str)]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return len(args[0])

# Operations on dates.

class Year(EvalFunction):
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].year

class Month(EvalFunction):
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].month

class Day(EvalFunction):
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].day

# Operation on inventories.

class Units(EvalFunction):
    __intypes__ = [(position.Position, inventory.Inventory)]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].get_amount()

class Cost(EvalFunction):
    __intypes__ = [(position.Position, inventory.Inventory)]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

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

class EvalAggregator(EvalFunction):
    "Base class for all aggregator evaluator types."


class Sum(EvalAggregator):
    # FIXME: Not sure we should accept a position.
    __intypes__ = [(int, float, Decimal, position.Position, inventory.Inventory)]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def __call__(self, posting):
        args = self.eval_args(posting)
        value = args[0]
        if self.state is None:
            if isinstance(value, position.Position):
                self.state = inventory.Inventory()
            else:
                self.state = type(value)()
        self.state += value

class Count(EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        if self.state is None:
            self.state = 0
        self.state += 1

class First(EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def __call__(self, posting):
        if self.state is None:
            args = self.eval_args(posting)
            self.state = args[0]

class Last(EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def __call__(self, posting):
        args = self.eval_args(posting)
        self.state = args[0]

AGGREGATOR_FUNCTIONS = {
    'sum': Sum,
    'count': Count,
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

class EvalColumn(EvalNode):
    "Base class for all column accessors."

class TypeEntryColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return type(entry).__name__

class FilenameEntryColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return entry.source.filename

class LineNoEntryColumn(EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, entry):
        return entry.source.lineno

class DateEntryColumn(EvalColumn):
    def __init__(self):
        super().__init__(datetime.date)

    def __call__(self, entry):
        return entry.date

class FlagEntryColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return entry.flag

class PayeeEntryColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return entry.payee or ''

class NarrationEntryColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return entry.narration

class TagsEntryColumn(EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, entry):
        return entry.tags

class LinksEntryColumn(EvalColumn):
    def __init__(self):
        super().__init__(set)

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

class TypeColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return type(posting.entry).__name__

class FilenameColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.entry.source.filename

class LineNoColumn(EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, posting):
        return posting.entry.source.lineno

class DateColumn(EvalColumn):
    def __init__(self):
        super().__init__(datetime.date)

    def __call__(self, posting):
        return posting.entry.date

class FlagColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.entry.flag

class PayeeColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.entry.payee or ''

class NarrationColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.entry.narration

class TagsColumn(EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, posting):
        return posting.entry.tags

class LinksColumn(EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, posting):
        return posting.entry.links

class AccountColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.account

class NumberColumn(EvalColumn):
    def __init__(self):
        super().__init__(Decimal)

    def __call__(self, posting):
        return posting.position.number

class CurrencyColumn(EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.position.lot.currency

class ChangeColumn(EvalColumn):
    def __init__(self):
        super().__init__(position.Position)

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



class AttributeColumn(EvalColumn):
    def __call__(self, row):
        return getattr(row, self.name)

class ResultSetContext(CompilationContext):
    """An execution context that provides access to attributes from a result set.
    """
    context_name = 'sub-query'

    def get_column(self, name):
        """Override the column getter to provide a single attribute getter.
        """
        # FIXME: How do we figure out the data type here? We need the context.
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
        # if len(c_operands) != len(c_expr.__intypes__):
        #     raise CompilationError(
        #         "Invalid number of arguments for {}: {} expected {}".format(
        #             expr.fname, len(c_operands), len(c_expr.__intypes__)))

    elif isinstance(expr, query_parser.UnaryOp):
        node_type = OPERATORS[type(expr)]
        c_expr = node_type(compile_expression(expr.operand, xcontext))

    elif isinstance(expr, query_parser.BinaryOp):
        node_type = OPERATORS[type(expr)]
        c_expr = node_type(compile_expression(expr.left, xcontext),
                           compile_expression(expr.right, xcontext))

    elif isinstance(expr, query_parser.Constant):
        c_expr = EvalConstant(expr.value)

    else:
        assert False, "Invalid expression to compile: {}".format(expr)

    return c_expr


def is_aggregate(node):
    """Return true if the given node is derived from an aggregate.

    Args:
      node: An instance of EvalNode.
    Returns:
      A boolean.
    """
    if isinstance(node, EvalAggregator):
        return True

    for child in node.childnodes():
        if is_aggregate(child):
            return True

    return False


def has_nested_aggregates(node, under_aggregate=False):
    """Check if the expression contains nested aggregates.

    Nested aggregates - aggregates of aggregates - should be disallowed. This
    function checks for their presence under the given evaluator node.

    Args:
      node: An instance of EvalNode.
      under_aggregate: A boolean, True if one of the parent nodes of this node
        is an aggregate evaluator.
    Returns:
      A boolean, true if the the expression contains a nested aggregate.
    """
    is_aggregate = isinstance(node, EvalAggregator)
    if under_aggregate and is_aggregate:
        return True

    for child in node.childnodes():
        if has_nested_aggregates(child, under_aggregate|is_aggregate):
            return True

    return False


def get_columns_and_aggregates(node):
    """Find the columns and aggregate nodes below this tree.

    All nodes under aggregate nodes are ignored.

    Args:
      node: An instance of EvalNode.
    Returns:
      A pair of (columns, aggregates), both of which are lists of EvalNode instances.
        columns: The list of all columns accessed not under an aggregate node.
        aggregates: The lis tof all aggregate nodes.
    """
    columns = []
    aggregates = []
    _get_columns_and_aggregates(node, columns, aggregates)
    return columns, aggregates


def _get_columns_and_aggregates(node, columns, aggregates):
    """Walk down a tree of nodes and fetch the column accessors and aggregates.

    This function ignores all nodes under aggregate nodes.

    Args:
      node: An instance of EvalNode.
      columns: An accumulator for columns found so far.
      aggregate: An accumulator for aggregate notes found so far.
    """
    if isinstance(node, EvalAggregator):
        aggregates.append(node)
    elif isinstance(node, EvalColumn):
        columns.append(node)
    else:
        for child in node.childnodes():
            _get_columns_and_aggregates(child, columns, aggregates)


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

    # Check that aggregations of aggregations are not possible.



    # Is this an aggregated query or not?

    ##is_query_aggregate = any(is_aggregate(c_target.expression)
    ##                         for c_target in c_targets)
    ##print('is_query_aggregate', is_query_aggregate)

    # for c_target in c_targets:
    #     print(c_target)
    #     print('is_query_aggregate', is_aggregate(c_target.expression))
    #     x = infer_types(c_target)
    #     print(x)
    #     print()

    # FIXME: Also check for the presence of a gruup-by clause

    # Check that the group-by column references are valid w.r.t. aggregates.
    # Check that the pivot-by column references are valid.
    # Check that the order-by column references are valid.




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


# FIXME: Add checking of datatypes. Something simple is better than nothing, but
# something, please, will avoid many types of user mistakes.
