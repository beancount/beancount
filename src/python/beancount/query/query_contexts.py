"""Context object for compiler.

This module contains the various column accessors and function evaluators that
are made available by the query compiler via their compilation context objects.
Define new columns and functions here.
"""
import collections
import copy
import datetime
import itertools
import io
import re
import operator

from beancount.core.amount import Decimal
from beancount.core.data import Transaction
from beancount.core import position
from beancount.core import inventory
from beancount.core import data
from beancount.query import query_compile as c




# Non-agreggating functions. These functionals maintain no state.

class Length(c.EvalFunction):
    __intypes__ = [(list, set, str)]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return len(args[0])

class Str(c.EvalFunction):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return repr(args[0])


# Operations on dates.

class Year(c.EvalFunction):
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].year

class Month(c.EvalFunction):
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].month

class Day(c.EvalFunction):
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].day


# Operation on inventories.

class Units(c.EvalFunction):
    __intypes__ = [(position.Position, inventory.Inventory)]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].get_amount()

class Cost(c.EvalFunction):
    __intypes__ = [(position.Position, inventory.Inventory)]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].get_cost()

SIMPLE_FUNCTIONS = {
    'str': Str,
    'length': Length,
    'units': Units,
    'cost': Cost,
    'year': Year,
    'month': Month,
    'day': Day,
    }




# Aggregating functions. These instances themselves both make the computation
# and manage state for a single iteration.

class Count(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, posting):
        if self.state is None:
            self.state = 0
        self.state += 1

class Sum(c.EvalAggregator):
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

class First(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def __call__(self, posting):
        if self.state is None:
            args = self.eval_args(posting)
            self.state = args[0]

class Last(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def __call__(self, posting):
        args = self.eval_args(posting)
        self.state = args[0]

class Min(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def __call__(self, posting):
        arg = self.eval_args(posting)[0]
        if self.state is None:
            self.state = arg
        elif arg < self.state:
            self.state = arg

class Max(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def __call__(self, posting):
        arg = self.eval_args(posting)[0]
        if self.state is None:
            self.state = arg
        elif arg > self.state:
            self.state = arg

AGGREGATOR_FUNCTIONS = {
    'sum': Sum,
    'count': Count,
    'first': First,
    'last': Last,
    'min': Min,
    'max': Max,
    }




# Column accessors for entries.

class TypeEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return type(entry).__name__.lower()

class FilenameEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return entry.source.filename

class LineNoEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, entry):
        return entry.source.lineno

class DateEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(datetime.date)

    def __call__(self, entry):
        return entry.date

class FlagEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return (entry.flag
                if isinstance(entry, Transaction)
                else None)

class PayeeEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return (entry.payee or ''
                if isinstance(entry, Transaction)
                else None)

class NarrationEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return (entry.narration or ''
                if isinstance(entry, Transaction)
                else None)

class TagsEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, entry):
        return (entry.tags
                if isinstance(entry, Transaction)
                else None)

class LinksEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, entry):
        return (entry.links
                if isinstance(entry, Transaction)
                else None)

class FilterEntriesContext(c.CompilationContext):
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

class TypeColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return type(posting.entry).__name__.lower()

class FilenameColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.entry.source.filename

class LineNoColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, posting):
        return posting.entry.source.lineno

class DateColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(datetime.date)

    def __call__(self, posting):
        return posting.entry.date

class FlagColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.entry.flag

class PayeeColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.entry.payee or ''

class NarrationColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.entry.narration

class TagsColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, posting):
        return posting.entry.tags

class LinksColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, posting):
        return posting.entry.links

class AccountColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.account

class NumberColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(Decimal)

    def __call__(self, posting):
        return posting.position.number

class CurrencyColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.position.lot.currency

class ChangeColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(position.Position)

    def __call__(self, posting):
        return posting.position

class FilterPostingsContext(c.CompilationContext):
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
