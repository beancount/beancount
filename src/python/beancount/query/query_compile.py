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
import operator

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


class EvalConstant(EvalNode):
    __slots__ = ('value',)

    def __init__(self, value):
        super().__init__(type(value))
        self.value = value

    def __call__(self, _):
        return self.value


class EvalUnaryOp(EvalNode):
    __slots__ = ('operand', 'operator')

    def __init__(self, operator, operand, dtype):
        super().__init__(dtype)
        self.operand = operand
        self.operator = operator

    def __call__(self, context):
        return self.operator(self.operand(context))

class EvalNot(EvalUnaryOp):

    def __init__(self, operand):
        super().__init__(operator.not_, operand, bool)


class EvalBinaryOp(EvalNode):
    __slots__ = ('left', 'right', 'operator')

    def __init__(self, operator, left, right, dtype):
        super().__init__(dtype)
        self.operator = operator
        self.left = left
        self.right = right

    def __call__(self, context):
        return self.operator(self.left(context) and self.right(context))

class EvalEqual(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(operator.eq, left, right, bool)

class EvalAnd(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(operator.and_, left, right, bool)

class EvalOr(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(operator.or_, left, right, bool)

class EvalGreater(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(operator.gt, left, right, bool)

class EvalGreaterEq(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(operator.ge, left, right, bool)

class EvalLess(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(operator.lt, left, right, bool)

class EvalLessEq(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(operator.le, left, right, bool)

class EvalMatch(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(re.search, left, right, bool)
        if right.dtype != str:
            raise CompilationError(
                "Invalid data type for RHS of match: '{}'; must be a string".format(
                    right.dtype))


# Interpreter nodes.
OPERATORS = {
    query_parser.Constant: EvalConstant,
    query_parser.Not: EvalNot,
    query_parser.Equal: EvalEqual,
    query_parser.Match: EvalMatch,
    query_parser.And: EvalAnd,
    query_parser.Or: EvalOr,
    query_parser.Greater: EvalGreater,
    query_parser.GreaterEq: EvalGreaterEq,
    query_parser.Less: EvalLess,
    query_parser.LessEq: EvalLessEq,
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
    # FIXME: Not sure we should accept a position. Compile a special node for Sum(position) and Sum(inventory).
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


def find_unique_name(name, allocated_set):
    """Come up with a unique name for 'name' amongst 'allocated_set'.

    Args:
      name: A string, the prefix of the name to find a unique for.
      allocated_set: A set of string, the set of already allocated names.
    Returns:
      A unique name. 'allocated_set' is unmodified.
    """
    # Make sure the name is unique.
    prefix = name
    i = 1
    while name in allocated_set:
        name = '{}_{}'.format(prefix, i)
        i += 1
    return name


def compile_targets(targets, xcontext):
    """Compile the targets and check for their validity. Process wildcard.

    Args:
      targets: A list of target expressions from the parser.
      xcontext: A compilation context for the targets.
    Returns:
      A tuple of
        c_targets: A list of compiled target expressions with resolved names.
        simple_indexes: A list of integer indexes for the non-aggregate targets.
        aggregate_indexes: A list of integer indexes for the aggregate targetsr.
    """
    # Bind the targets expressions to the execution context.
    if isinstance(targets, query_parser.Wildcard):
        # Insert the full list of available columns.
        targets = [query_parser.Target(query_parser.Column(name), None)
                   for name in TargetsContext.columns]

    # Compile targets.
    c_targets = []
    target_names = set()
    for target in targets:
        c_expr = compile_expression(target.expression, xcontext)
        target_name = find_unique_name(
            target.name or query_parser.get_expression_name(target.expression),
            target_names)
        target_names.add(target_name)
        c_targets.append(query_parser.Target(c_expr, target_name))

    # Figure out if this query is an aggregate query and check validity of each
    # target's aggregation type.
    simple_indexes = []
    aggregate_indexes = []
    for index, c_target in enumerate(c_targets):
        columns, aggregates = get_columns_and_aggregates(c_target.expression)

        # Check for mixed aggregates and non-aggregates.
        if columns and aggregates:
            raise CompilationError(
                "Mixed aggregates and non-aggregates are not allowed")

        if not aggregates:
            simple_indexes.append(index)
        else:
            aggregate_indexes.append(index)

            # Check for aggregates of aggregates.
            for aggregate in aggregates:
                for child in aggregate.childnodes():
                    _, sub_aggregates = get_columns_and_aggregates(child)
                    if sub_aggregates:
                        raise CompilationError(
                            "Aggregates of aggregates are not allowed")

    return c_targets, simple_indexes, aggregate_indexes


def compile_group_by(group_by, c_targets, xcontext, simple_indexes, aggregate_indexes):
    """Process a group-by clause.

    Args:
      group_by: A GroupBy instance as provided by the parser.
      c_targets: A list of compiled target expressions.
      xcontext: A compilation context to be used to evaluate GROUP BY expressions.
      simple_indexes: A list of integer indexes for the non-aggregate targetsr.
      aggregate_indexes: A list of integer indexes for the aggregate targetsr.
    Returns:
      A tuple of
       new_targets: A list of new compiled target nodes.
       new_indexes: A list of integer indexes that refer to the newly inserted targets.
       group_indexes: A list of integer indexes to be used for processing grouping.
    """
    new_targets = copy.copy(c_targets)
    new_indexes = []
    group_indexes = []
    if group_by:
        # Check that HAVING is not supported yet.
        if group_by and group_by.having is not None:
            raise CompilationError("The HAVING clause is not supported yet")

        assert group_by.columns, "Internal error with GROUP-BY parsing"

        # Compile group-by expressions and resolve them to their targets if
        # possible. A GROUP-BY column may be one of the following:
        #
        # * A reference to a target by name.
        # * A reference to a target by index (starting at one).
        # * A new, non-aggregate expression.
        #
        # References by name are converted to indexes. New expressions are
        # inserted into the list of targets as invisible targets.
        targets_name_map = {target.name: index
                            for index, target in enumerate(c_targets)}
        for column in group_by.columns:
            index = None

            # Process target references by index.
            if isinstance(column, int):
                index = column - 1
                if not (0 <= index < len(c_targets)):
                    raise CompilationError(
                        "Invalid GROUP-BY column index {}".format(column))

            else:
                # Process target references by name. These will be parsed as
                # simple Column expressions. If they refer to a target name, we
                # resolve them.
                if isinstance(column, query_parser.Column):
                    name = column.name
                    index = targets_name_map.get(name, None)

                # Otherwise we compile the expression and add it to the list of
                # targets to evaluate and index into that new target.
                if index is None:
                    c_expr = compile_expression(column, xcontext)

                    # Check if the new expression is an aggregate.
                    _, aggregates = get_columns_and_aggregates(c_expr)
                    if aggregates:
                        raise CompilationError(
                            "GROUP-BY expressions may not be aggregates: '{}'".format(
                                column))

                    # Add the new target. 'None' for the target name implies it
                    # should be invisible, not to be rendered.
                    index = len(new_targets)
                    new_targets.append(query_parser.Target(c_expr, None))
                    new_indexes.append(index)

            assert index is not None, "Internal error, could not index group-by reference."
            group_indexes.append(index)

            # Check that the group-by column references a non-aggregate.
            c_expr = new_targets[index].expression
            _, aggregates = get_columns_and_aggregates(c_expr)
            if aggregates:
                raise CompilationError(
                    "GROUP-BY expressions may not reference aggregates: '{}'".format(
                        column))

    else:
        # If it does not have a GROUP-BY clause...
        if aggregate_indexes:
            # If the query is an aggregate query, check that all the targets are
            # aggregates.
            if simple_indexes:
                raise CompilationError(
                    "Aggregate query without a GROUP-BY should have only aggregates")

        else:
            # This is not an aggregate query; don't set group_indexes to
            # anything useful, we won't need it.
            group_indexes = None

    return new_targets[len(c_targets):], new_indexes, group_indexes


def compile_order_by(order_by, c_targets, xcontext):
    """Process an order-by clause.

    Args:
      order_by: A OrderBy instance as provided by the parser.
      c_targets: A list of compiled target expressions.
      xcontext: A compilation context to be used to evaluate ORDER BY expressions.
    Returns:
      A tuple of
       new_targets: A list of new compiled target nodes.
       new_simple_indexes: A list of integer indexes that refer to the newly inserted
         non-aggregate targets.
       new_aggregate_indexes: A list of integer indexes that refer to the newly inserted
         aggregatetargets.
       order_indexes: A list of integer indexes to be used for processing ordering.
    """
    new_targets = copy.copy(c_targets)
    new_simple_indexes = []
    new_aggregate_indexes = []
    order_indexes = []

    # Compile order-by expressions and resolve them to their targets if
    # possible. A ORDER-BY column may be one of the following:
    #
    # * A reference to a target by name.
    # * A reference to a target by index (starting at one).
    # * A new expression, aggregate or not.
    #
    # References by name are converted to indexes. New expressions are
    # inserted into the list of targets as invisible targets.
    targets_name_map = {target.name: index
                        for index, target in enumerate(c_targets)}
    for column in order_by.columns:
        index = None

        # Process target references by index.
        if isinstance(column, int):
            index = column - 1
            if not (0 <= index < len(c_targets)):
                raise CompilationError(
                    "Invalid ORDER-BY column index {}".format(column))

        else:
            # Process target references by name. These will be parsed as
            # simple Column expressions. If they refer to a target name, we
            # resolve them.
            if isinstance(column, query_parser.Column):
                name = column.name
                index = targets_name_map.get(name, None)

            # Otherwise we compile the expression and add it to the list of
            # targets to evaluate and index into that new target.
            if index is None:
                c_expr = compile_expression(column, xcontext)

                # Add the new target. 'None' for the target name implies it
                # should be invisible, not to be rendered.
                index = len(new_targets)
                new_targets.append(query_parser.Target(c_expr, None))

                _, aggregates = get_columns_and_aggregates(c_expr)
                if aggregates:
                    new_aggregate_indexes.append(index)
                else:
                    new_simple_indexes.append(index)

        assert index is not None, "Internal error, could not index order-by reference."
        order_indexes.append(index)

    return (new_targets[len(c_targets):],
            new_simple_indexes,
            new_aggregate_indexes,
            order_indexes)


# Compiled query.
EvalQuery = collections.namedtuple(
    'EvalQuery', ('c_targets c_from c_where '
                  'simple_indexes aggregate_indexes group_indexes order_indexes '
                  'limit distinct flatten'))


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

    # Check that the from clause does not contain aggregates.
    if c_from and c_from.expression:
        _, aggregates = get_columns_and_aggregates(c_from.expression)
        if aggregates:
            raise CompilationError(
                "Aggregates are not allowed in from clause")

    # Compile the targets.
    c_targets, simple_indexes, aggregate_indexes = compile_targets(select.targets,
                                                                   xcontext_target)

    # Process the group-by clause.
    new_targets, new_indexes, group_indexes = compile_group_by(
        select.group_by,
        c_targets, xcontext_target,
        simple_indexes, aggregate_indexes)

    c_targets.extend(new_targets)
    simple_indexes.extend(new_indexes)

    # If this is an aggregate query (it has some aggregates), check that the set
    # of non-aggregates match exactly the group indexes. This should always be
    # the case at this point, because we have added all the necessary targets to
    # the list of group-by expressions and should have resolved all the indexes.
    if aggregate_indexes:
        if set(simple_indexes) != set(group_indexes):
            raise CompilationError(
                "Non-aggregates must be covered by GROUP-BY clause in aggregate query")

    # Bind the WHERE expression to the execution context.
    # Note: Aggregates are disallowed in this clause.
    # FIXME: check for this!
    c_where = (compile_expression(select.where_clause, xcontext_where)
               if select.where_clause is not None
               else None)

    # Check that ORDER-BY is not supported yet.
    if select.order_by is not None:
        (new_targets,
         new_simple_indexes,
         new_aggregate_indexes,
         order_indexes) = compile_order_by(select.order_by,
                                           c_targets, xcontext_target)

        c_targets.extend(new_targets)
        simple_indexes.extend(new_simple_indexes)
        aggregate_indexes.extend(new_aggregate_indexes)
    else:
        order_indexes = None

    # Check that PIVOT-BY is not supported yet.
    if select.pivot_by is not None:
        raise CompilationError("The PIVOT BY clause is not supported yet")

    return EvalQuery(c_targets,
                     c_from,
                     c_where,
                     simple_indexes,
                     aggregate_indexes,
                     group_indexes,
                     order_indexes,
                     select.limit,
                     select.distinct,
                     select.flatten)








# FIXME: Deal with sum of inventory, compile special nodes SumPosition(), SumInventory().
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
