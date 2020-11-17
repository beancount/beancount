"""Interpreter for the query language's AST.

This code accepts the abstract syntax tree produced by the query parser,
resolves the column and function names, compiles and interpreter and prepares a
query to be run against a list of entries.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import copy
import datetime
import re
import operator
from decimal import Decimal

from beancount.core import inventory
from beancount.query import query_parser


# A global constant which sets whether we support inferred/implicit group-by
# semantics.
SUPPORT_IMPLICIT_GROUPBY = True


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

    def __call__(self, context):
        """Evaluate this node. This is designed to recurse on its children.
        All subclasses must override and implement this method.

        Args:
          context: The evaluation object to which the evaluation need to apply.
            This is either an entry, a Posting instance, or a particular result
            set row from a sub-select. This is the provider for the underlying
            data.
        Returns:
          The evaluated value for this sub-expression tree.
        """
        raise NotImplementedError


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
        return self.operator(self.left(context), self.right(context))

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

    @staticmethod
    def match(left, right):
        if left is None or right is None:
            return False
        return bool(re.search(right, left, re.IGNORECASE))

    def __init__(self, left, right):
        super().__init__(self.match, left, right, bool)
        if right.dtype != str:
            raise CompilationError(
                "Invalid data type for RHS of match: '{}'; must be a string".format(
                    right.dtype))

class EvalContains(EvalBinaryOp):

    def __init__(self, left, right):
        super().__init__(operator.contains, left, right, bool)

    def __call__(self, context):
        # Note: we need to reverse the arguments.
        arg_left = self.left(context)
        arg_right = self.right(context)
        return self.operator(arg_right, arg_left)


# Note: We ought to implement implicit type promotion here,
# e.g., int -> float -> Decimal.

# Note(2): This does not support multiplication on Amount, Position, Inventory.
# We need to rewrite the evaluator to support types in order to do this
# properly.

class EvalMul(EvalBinaryOp):

    def __init__(self, left, right):
        f = lambda x, y: Decimal(x * y)
        super().__init__(f, left, right, Decimal)

class EvalDiv(EvalBinaryOp):

    def __init__(self, left, right):
        f = lambda x, y: Decimal(x / y)
        super().__init__(f, left, right, Decimal)

class EvalAdd(EvalBinaryOp):

    def __init__(self, left, right):
        f = lambda x, y: Decimal(x + y)
        super().__init__(f, left, right, Decimal)

class EvalSub(EvalBinaryOp):

    def __init__(self, left, right):
        f = lambda x, y: Decimal(x - y)
        super().__init__(f, left, right, Decimal)


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
    query_parser.Contains: EvalContains,
    query_parser.Mul: EvalMul,
    query_parser.Div: EvalDiv,
    query_parser.Add: EvalAdd,
    query_parser.Sub: EvalSub,
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


class EvalColumn(EvalNode):
    "Base class for all column accessors."

class EvalAggregator(EvalFunction):
    "Base class for all aggregator evaluator types."

    # We should not have to recurse any further because there should be no
    # aggregations under an aggregation node.

    def allocate(self, allocator):
        """Allocate handles to store data for a node's aggregate storage.

        This is called once before beginning aggregations. If you need any
        kind of per-aggregate storage during the computation phase, get it
        in this method.

        Args:
          allocator: An instance of Allocator, on which you can call allocate() to
            obtain a handle for a slot to store data on store objects later on.
        """
        # Do nothing by default.

    def initialize(self, store):
        """Initialize this node's aggregate data. If the node is not an aggregate,
        simply initialize the subnodes. Override this method in the aggregator
        if you need data for storage.

        Args:
          store: An object indexable by handles appropriated during allocate().
        """
        # Do nothing by default.

    def update(self, store, context):
        """Evaluate this node. This is designed to recurse on its children.

        Args:
          store: An object indexable by handles appropriated during allocate().
          context: The object to which the evaluation need to apply (see __call__).
        """
        # Do nothing by default.

    def finalize(self, store):
        """Finalize this node's aggregate data and return it.

        For aggregate methods, this finalizes the node and returns the final
        value. The context node will be the alloc instead of the context object.

        Args:
          store: An object indexable by handles appropriated during allocate().
        """
        # Do nothing by default.

    def __call__(self, context):
        """Return the value on evaluation.

        Args:
          context: The evaluation object to which the evaluation need to apply.
            This is either an entry, a Posting instance, or a particular result
            set row from a sub-select. This is the provider for the underlying
            data.
        Returns:
          The final aggregated value.
        """
        # Return None by default.


class CompilationEnvironment:
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
        except KeyError as exc:
            raise CompilationError("Invalid column name '{}' in {} context.".format(
                name, self.context_name)) from exc

    def get_function(self, name, operands):
        """Return a function accessor for the given named function.
        Args:
          name: A string, the name of the function to access.
        """
        try:
            key = tuple([name] + [operand.dtype for operand in operands])
            return self.functions[key](operands)
        except KeyError:
            # If not found with the operands, try just looking it up by name.
            try:
                return self.functions[name](operands)
            except KeyError as exc:
                signature = '{}({})'.format(name,
                                            ', '.join(operand.dtype.__name__
                                                      for operand in operands))
                raise CompilationError("Invalid function '{}' in {} context".format(
                    signature, self.context_name)) from exc


class AttributeColumn(EvalColumn):
    def __call__(self, row):
        return getattr(row, self.name)

class ResultSetEnvironment(CompilationEnvironment):
    """An execution context that provides access to attributes from a result set.
    """
    context_name = 'sub-query'

    def get_column(self, name):
        """Override the column getter to provide a single attribute getter.
        """
        # FIXME: How do we figure out the data type here? We need the context.
        return AttributeColumn(name)


def compile_expression(expr, environ):
    """Bind an expression to its execution context.

    Args:
      expr: The root node of an expression.
      environ: An CompilationEnvironment instance.
    Returns:
      The root node of a bound expression.
    """
    # Convert column references to the context.
    if isinstance(expr, query_parser.Column):
        c_expr = environ.get_column(expr.name)

    elif isinstance(expr, query_parser.Function):
        c_operands = [compile_expression(operand, environ)
                      for operand in expr.operands]
        c_expr = environ.get_function(expr.fname, c_operands)

    elif isinstance(expr, query_parser.UnaryOp):
        node_type = OPERATORS[type(expr)]
        c_expr = node_type(compile_expression(expr.operand, environ))

    elif isinstance(expr, query_parser.BinaryOp):
        node_type = OPERATORS[type(expr)]
        c_expr = node_type(compile_expression(expr.left, environ),
                           compile_expression(expr.right, environ))

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
        aggregates: The list of all aggregate nodes.
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


def is_aggregate(node):
    """Return true if the node is an aggregate.

    Args:
      node: An instance of EvalNode.
    Returns:
      A boolean.
    """
    # Note: We could be a tiny bit more efficient here, but it doesn't matter
    # much. Performance of the query compilation matters very little overall.
    _, aggregates = get_columns_and_aggregates(node)
    return bool(aggregates)


def is_hashable_type(node):
    """Return true if the node is of a hashable type.

    Args:
      node: An instance of EvalNode.
    Returns:
      A boolean.
    """
    return not issubclass(node.dtype, inventory.Inventory)


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


# A compiled target.
#
# Attributes:
#   c_expr: A compiled expression tree (an EvalNode root node).
#   name: The name of the target. If None, this is an invisible
#     target that gets evaluated but not displayed.
#   is_aggregate: A boolean, true if 'c_expr' is an aggregate.
EvalTarget = collections.namedtuple('EvalTarget', 'c_expr name is_aggregate')

def compile_targets(targets, environ):
    """Compile the targets and check for their validity. Process wildcard.

    Args:
      targets: A list of target expressions from the parser.
      environ: A compilation context for the targets.
    Returns:
      A list of compiled target expressions with resolved names.
    """
    # Bind the targets expressions to the execution context.
    if isinstance(targets, query_parser.Wildcard):
        # Insert the full list of available columns.
        targets = [query_parser.Target(query_parser.Column(name), None)
                   for name in environ.wildcard_columns]

    # Compile targets.
    c_targets = []
    target_names = set()
    for target in targets:
        c_expr = compile_expression(target.expression, environ)
        target_name = find_unique_name(
            target.name or query_parser.get_expression_name(target.expression),
            target_names)
        target_names.add(target_name)
        c_targets.append(EvalTarget(c_expr, target_name, is_aggregate(c_expr)))

    # Figure out if this query is an aggregate query and check validity of each
    # target's aggregation type.
    for index, c_target in enumerate(c_targets):
        columns, aggregates = get_columns_and_aggregates(c_target.c_expr)

        # Check for mixed aggregates and non-aggregates.
        if columns and aggregates:
            raise CompilationError(
                "Mixed aggregates and non-aggregates are not allowed")

        if aggregates:
            # Check for aggregates of aggregates.
            for aggregate in aggregates:
                for child in aggregate.childnodes():
                    if is_aggregate(child):
                        raise CompilationError(
                            "Aggregates of aggregates are not allowed")

    return c_targets


def compile_group_by(group_by, c_targets, environ):
    """Process a group-by clause.

    Args:
      group_by: A GroupBy instance as provided by the parser.
      c_targets: A list of compiled target expressions.
      environ: A compilation context to be used to evaluate GROUP BY expressions.
    Returns:
      A tuple of
       new_targets: A list of new compiled target nodes.
       group_indexes: If the query is an aggregate query, a list of integer
         indexes to be used for processing grouping. Note that this list may be
         empty (in the case of targets with only aggregates). On the other hand,
         if this is not an aggregated query, this is set to None. So do
         distinguish the empty list vs. None.
    """
    new_targets = copy.copy(c_targets)
    c_target_expressions = [c_target.c_expr for c_target in c_targets]

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
                    c_expr = compile_expression(column, environ)

                    # Check if the new expression is an aggregate.
                    aggregate = is_aggregate(c_expr)
                    if aggregate:
                        raise CompilationError(
                            "GROUP-BY expressions may not be aggregates: '{}'".format(
                                column))

                    # Attempt to reconcile the expression with one of the existing
                    # target expressions.
                    try:
                        index = c_target_expressions.index(c_expr)
                    except ValueError:
                        # Add the new target. 'None' for the target name implies it
                        # should be invisible, not to be rendered.
                        index = len(new_targets)
                        new_targets.append(EvalTarget(c_expr, None, aggregate))
                        c_target_expressions.append(c_expr)

            assert index is not None, "Internal error, could not index group-by reference."
            group_indexes.append(index)

            # Check that the group-by column references a non-aggregate.
            c_expr = new_targets[index].c_expr
            if is_aggregate(c_expr):
                raise CompilationError(
                    "GROUP-BY expressions may not reference aggregates: '{}'".format(
                        column))

            # Check that the group-by column has a supported hashable type.
            if not is_hashable_type(c_expr):
                raise CompilationError(
                    "GROUP-BY a non-hashable type is not supported: '{}'".format(
                        column))


    else:
        # If it does not have a GROUP-BY clause...
        aggregate_bools = [c_target.is_aggregate for c_target in c_targets]
        if any(aggregate_bools):
            # If the query is an aggregate query, check that all the targets are
            # aggregates.
            if all(aggregate_bools):
                assert group_indexes == []
            else:
                # If some of the targets aren't aggregates, automatically infer
                # that they are to be implicit group by targets. This makes for
                # a much more convenient syntax for our lightweight SQL, where
                # grouping is optional.
                if SUPPORT_IMPLICIT_GROUPBY:
                    group_indexes = [index
                                     for index, c_target in enumerate(c_targets)
                                     if not c_target.is_aggregate]
                else:
                    raise CompilationError(
                        "Aggregate query without a GROUP-BY should have only aggregates")
        else:
            # This is not an aggregate query; don't set group_indexes to
            # anything useful, we won't need it.
            group_indexes = None

    return new_targets[len(c_targets):], group_indexes


def compile_order_by(order_by, c_targets, environ):
    """Process an order-by clause.

    Args:
      order_by: A OrderBy instance as provided by the parser.
      c_targets: A list of compiled target expressions.
      environ: A compilation context to be used to evaluate ORDER BY expressions.
    Returns:
      A tuple of
       new_targets: A list of new compiled target nodes.
       order_indexes: A list of integer indexes to be used for processing ordering.
    """
    new_targets = copy.copy(c_targets)
    c_target_expressions = [c_target.c_expr for c_target in c_targets]
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
                c_expr = compile_expression(column, environ)

                # Attempt to reconcile the expression with one of the existing
                # target expressions.
                try:
                    index = c_target_expressions.index(c_expr)
                except ValueError:
                    # Add the new target. 'None' for the target name implies it
                    # should be invisible, not to be rendered.
                    index = len(new_targets)
                    new_targets.append(EvalTarget(c_expr, None, is_aggregate(c_expr)))
                    c_target_expressions.append(c_expr)

        assert index is not None, "Internal error, could not index order-by reference."
        order_indexes.append(index)

    return (new_targets[len(c_targets):], order_indexes)


# A compile FROM clause.
#
# Attributes:
#   c_expr: A compiled expression tree (an EvalNode root node).
#   close: (See query_parser.From.close).
EvalFrom = collections.namedtuple('EvalFrom', 'c_expr open close clear')

def compile_from(from_clause, environ):
    """Compiled a From clause as provided by the parser, in the given environment.

    Args:
      select: An instance of query_parser.Select.
      environ: : A compilation context for evaluating entry filters.
    Returns:
      An instance of Query, ready to be executed.
    """
    if from_clause is not None:
        c_expression = (compile_expression(from_clause.expression, environ)
                        if from_clause.expression is not None
                        else None)

        # Check that the from clause does not contain aggregates.
        if c_expression is not None and is_aggregate(c_expression):
            raise CompilationError("Aggregates are not allowed in from clause")

        if (isinstance(from_clause.open, datetime.date) and
            isinstance(from_clause.close, datetime.date) and
            from_clause.open > from_clause.close):
            raise CompilationError("Invalid dates: CLOSE date must follow OPEN date")

        c_from = EvalFrom(c_expression,
                          from_clause.open,
                          from_clause.close,
                          from_clause.clear)
    else:
        c_from = None

    return c_from


# A compiled query, ready for execution.
#
# Attributes:
#   c_targets: A list of compiled targets (instancef of EvalTarget).
#   c_from: An instance of EvalNode, a compiled expression tree, for directives.
#   c_where: An instance of EvalNode, a compiled expression tree, for postings.
#   group_indexes: A list of integers that describe which target indexes to
#     group by. All the targets referenced here should be non-aggregates. In fact,
#     this list of indexes should always cover all non-aggregates in 'c_targets'.
#     And this list may well include some invisible columns if only specified in
#     the GROUP BY clause.
#   order_indexes: A list of integers that describe which targets to order by.
#     This list may refer to either aggregates or non-aggregates.
#   limit: An optional integer used to cut off the number of result rows returned.
#   distinct: An optional boolean that requests we should uniquify the result rows.
#   flatten: An optional boolean that requests we should output a single posting
#     row for each currency present in an accumulated and output inventory.
EvalQuery = collections.namedtuple('EvalQuery', ('c_targets c_from c_where '
                                                 'group_indexes order_indexes ordering '
                                                 'limit distinct flatten'))

def compile_select(select, targets_environ, postings_environ, entries_environ):
    """Prepare an AST for a Select statement into a very rudimentary execution tree.
    The execution tree mostly looks much like an AST, but with some nodes
    replaced with knowledge specific to an execution context and eventually some
    basic optimizations.

    Args:
      select: An instance of query_parser.Select.
      targets_environ: A compilation environment for evaluating targets.
      postings_environ: A compilation environment for evaluating postings filters.
      entries_environ: A compilation environment for evaluating entry filters.
    Returns:
      An instance of EvalQuery, ready to be executed.
    """

    # Process the FROM clause and figure out the execution environment for the
    # targets and the where clause.
    from_clause = select.from_clause
    if isinstance(from_clause, query_parser.Select):
        c_from = None
        environ_target = ResultSetEnvironment()
        environ_where = ResultSetEnvironment()

        # Remove this when we add support for nested queries.
        raise CompilationError("Queries from nested SELECT are not supported yet")

    if from_clause is None or isinstance(from_clause, query_parser.From):
        # Bind the from clause contents.
        c_from = compile_from(from_clause, entries_environ)
        environ_target = targets_environ
        environ_where = postings_environ

    else:
        raise CompilationError("Unexpected from clause in AST: {}".format(from_clause))

    # Compile the targets.
    c_targets = compile_targets(select.targets, environ_target)

    # Bind the WHERE expression to the execution environment.
    if select.where_clause is not None:
        c_where = compile_expression(select.where_clause, environ_where)

        # Aggregates are disallowed in this clause. Check for this.
        # NOTE: This should never trigger if the compilation environment does not
        # contain any aggregate. Just being manic and safe here.
        if is_aggregate(c_where):
            raise CompilationError("Aggregates are disallowed in WHERE clause")
    else:
        c_where = None

    # Process the GROUP-BY clause.
    new_targets, group_indexes = compile_group_by(select.group_by,
                                                  c_targets,
                                                  environ_target)
    if new_targets:
        c_targets.extend(new_targets)

    # Process the ORDER-BY clause.
    if select.order_by is not None:
        (new_targets, order_indexes) = compile_order_by(select.order_by,
                                                        c_targets,
                                                        environ_target)
        if new_targets:
            c_targets.extend(new_targets)
        ordering = select.order_by.ordering
    else:
        order_indexes = None
        ordering = None

    # If this is an aggregate query (it groups, see list of indexes), check that
    # the set of non-aggregates match exactly the group indexes. This should
    # always be the case at this point, because we have added all the necessary
    # targets to the list of group-by expressions and should have resolved all
    # the indexes.
    if group_indexes is not None:
        non_aggregate_indexes = set(index
                                    for index, c_target in enumerate(c_targets)
                                    if not c_target.is_aggregate)
        if non_aggregate_indexes != set(group_indexes):
            missing_names = ['"{}"'.format(c_targets[index].name)
                             for index in non_aggregate_indexes - set(group_indexes)]
            raise CompilationError(
                "All non-aggregates must be covered by GROUP-BY clause in aggregate query; "
                "the following targets are missing: {}".format(",".join(missing_names)))

    # Check that PIVOT-BY is not supported yet.
    if select.pivot_by is not None:
        raise CompilationError("The PIVOT BY clause is not supported yet")

    return EvalQuery(c_targets,
                     c_from,
                     c_where,
                     group_indexes,
                     order_indexes,
                     ordering,
                     select.limit,
                     select.distinct,
                     select.flatten)


def transform_journal(journal):
    """Translate a Journal entry into an uncompiled Select statement.

    Args:
      journal: An instance of a Journal object.
    Returns:
      An instance of an uncompiled Select object.
    """
    cooked_select = query_parser.Parser().parse("""

        SELECT
           date,
           flag,
           MAXWIDTH(payee, 48),
           MAXWIDTH(narration, 80),
           account,
           {summary_func}(position),
           {summary_func}(balance)
        {where}

    """.format(where=('WHERE account ~ "{}"'.format(journal.account)
                      if journal.account
                      else ''),
               summary_func=journal.summary_func or ''))

    return query_parser.Select(cooked_select.targets,
                               journal.from_clause,
                               cooked_select.where_clause,
                               None, None, None, None, None, None)


def transform_balances(balances):
    """Translate a Balances entry into an uncompiled Select statement.

    Args:
      balances: An instance of a Balance object.
    Returns:
      An instance of an uncompiled Select object.
    """
    ## FIXME: Change the aggregation rules to allow GROUP-BY not to include the
    ## non-aggregate ORDER-BY columns, so we could just GROUP-BY accounts here
    ## instead of having to include the sort-key. I think it should be fine if
    ## the first or last sort-order value gets used, because it would simplify
    ## the input statement.

    cooked_select = query_parser.Parser().parse("""

      SELECT account, SUM({}(position))
      GROUP BY account, ACCOUNT_SORTKEY(account)
      ORDER BY ACCOUNT_SORTKEY(account)

    """.format(balances.summary_func or ""))

    return query_parser.Select(cooked_select.targets,
                               balances.from_clause,
                               balances.where_clause,
                               cooked_select.group_by,
                               cooked_select.order_by,
                               None, None, None, None)


# A compiled print statement, ready for execution.
#
# Attributes:
#   c_from: An instance of EvalNode, a compiled expression tree, for directives.
EvalPrint = collections.namedtuple('EvalPrint', 'c_from')

def compile_print(print_stmt, env_entries):
    """Compile a Print statement.

    Args:
      statement: An instance of query_parser.Print.
      entries_environ: : A compilation environment for evaluating entry filters.
    Returns:
      An instance of EvalPrint, ready to be executed.
    """
    c_from = compile_from(print_stmt.from_clause, env_entries)
    return EvalPrint(c_from)


def compile(statement, targets_environ, postings_environ, entries_environ):
    """Prepare an AST any of the statement into an executable statement.

    Args:
      statement: An instance of the parser's Select, Balances, Journal or Print.
      targets_environ: A compilation environment for evaluating targets.
      postings_environ: : A compilation environment for evaluating postings filters.
      entries_environ: : A compilation environment for evaluating entry filters.
    Returns:
      An instance of EvalQuery or EvalPrint, ready to be executed.
    Raises:
      CompilationError: If the statement cannot be compiled, or is not one of the
        supported statements.
    """
    if isinstance(statement, query_parser.Balances):
        statement = transform_balances(statement)
    elif isinstance(statement, query_parser.Journal):
        statement = transform_journal(statement)

    if isinstance(statement, query_parser.Select):
        c_query = compile_select(statement,
                                 targets_environ, postings_environ, entries_environ)
    elif isinstance(statement, query_parser.Print):
        c_query = compile_print(statement, entries_environ)
    else:
        raise CompilationError(
            "Cannot compile a statement of type '{}'".format(type(statement)))

    return c_query
