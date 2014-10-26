"""Environment object for compiler.

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

## FIXME: Specialize both of these.

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
    'str'    : Str,
    'length' : Length,
    'units'  : Units,
    'cost'   : Cost,
    'year'   : Year,
    'month'  : Month,
    'day'    : Day,
    }




# Aggregating functions. These instances themselves both make the computation
# and manage state for a single iteration.

class Count(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, int)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = 0

    def update(self, store, unused_ontext):
        store[self.handle] += 1

    def finalize(self, store):
        return store[self.handle]

class Sum(c.EvalAggregator):
    __intypes__ = [(int, float, Decimal)]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = self.dtype()

    def update(self, store, posting):
        value = self.eval_args(posting)[0]
        store[self.handle] += value

    def finalize(self, store):
        return store[self.handle]

class SumPosition(c.EvalAggregator):
    __intypes__ = [position.Position]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = inventory.Inventory()

    def update(self, store, posting):
        value = self.eval_args(posting)[0]
        store[self.handle].add_position(value)

    def finalize(self, store):
        return store[self.handle]

class First(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = None

    def update(self, store, posting):
        if store[self.handle] is None:
            value = self.eval_args(posting)[0]
            store[self.handle] = value

    def finalize(self, store):
        return store[self.handle]

class Last(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = None

    def update(self, store, posting):
        value = self.eval_args(posting)[0]
        store[self.handle] = value

    def finalize(self, store):
        return store[self.handle]

class Min(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = self.dtype()

    def update(self, store, posting):
        value = self.eval_args(posting)[0]
        if value < store[self.handle]:
            store[self.handle] = value

    def finalize(self, store):
        return store[self.handle]

class Max(c.EvalAggregator):
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = self.dtype()

    def update(self, store, posting):
        value = self.eval_args(posting)[0]
        if value > store[self.handle]:
            store[self.handle] = value

    def finalize(self, store):
        return store[self.handle]

AGGREGATOR_FUNCTIONS = {
    ('sum', position.Position) : SumPosition,
    ('sum', int)               : Sum,
    ('sum', float)             : Sum,
    ('sum', Decimal)           : Sum,
    'count'                    : Count,
    'first'                    : First,
    'last'                     : Last,
    'min'                      : Min,
    'max'                      : Max,
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

class YearEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, entry):
        return entry.date.year

class MonthEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, entry):
        return entry.date.month

class DayEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, entry):
        return entry.date.day

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

# A globally available empty set to fill in for None's.
EMPTY_SET = frozenset()

class TagsEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, entry):
        return (entry.tags or EMPTY_SET
                if isinstance(entry, Transaction)
                else EMPTY_SET)

class LinksEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, entry):
        return (entry.links or EMPTY_SET
                if isinstance(entry, Transaction)
                else EMPTY_SET)

class FilterEntriesEnvironment(c.CompilationEnvironment):
    """An execution context that provides access to attributes on Transactions
    and other entry types.
    """
    context_name = 'FROM clause'
    columns = {
        'type'      : TypeEntryColumn,
        'filename'  : FilenameEntryColumn,
        'lineno'    : LineNoEntryColumn,
        'date'      : DateEntryColumn,
        'year'      : YearEntryColumn,
        'month'     : MonthEntryColumn,
        'day'       : DayEntryColumn,
        'flag'      : FlagEntryColumn,
        'payee'     : PayeeEntryColumn,
        'narration' : NarrationEntryColumn,
        'tags'      : TagsEntryColumn,
        'links'     : LinksEntryColumn,
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

class YearColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, posting):
        return posting.entry.date.year

class MonthColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, posting):
        return posting.entry.date.month

class DayColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(int)

    def __call__(self, posting):
        return posting.entry.date.day

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

class FilterPostingsEnvironment(c.CompilationEnvironment):
    """An execution context that provides access to attributes on Postings.
    """
    context_name = 'WHERE clause'
    columns = {
        'type'      : TypeColumn,
        'filename'  : FilenameColumn,
        'lineno'    : LineNoColumn,
        'date'      : DateColumn,
        'year'      : YearColumn,
        'month'     : MonthColumn,
        'day'       : DayColumn,
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

class TargetsEnvironment(FilterPostingsEnvironment):
    """An execution context that provides access to attributes on Postings.
    """
    context_name = 'targets/column'
    functions = copy.copy(FilterPostingsEnvironment.functions)
    functions.update(AGGREGATOR_FUNCTIONS)
