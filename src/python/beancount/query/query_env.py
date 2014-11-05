"""Environment object for compiler.

This module contains the various column accessors and function evaluators that
are made available by the query compiler via their compilation context objects.
Define new columns and functions here.
"""
import copy
import datetime
import re

from beancount.core.amount import Decimal
from beancount.core.amount import ZERO
from beancount.core.data import Transaction
from beancount.core.compare import hash_entry
from beancount.core import complete
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.core import account
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

class Weekday(c.EvalFunction):
    "Outputs a 3-letter locale-specific code."
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].strftime('%a')


# Operations on accounts.

class Parent(c.EvalFunction):
    "Get the parent name of the account."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return account.parent(args[0])


# Operation on inventories.

class UnitsPosition(c.EvalFunction):
    __intypes__ = [position.Position]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].get_amount()

class CostPosition(c.EvalFunction):
    __intypes__ = [position.Position]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, posting):
        args = self.eval_args(posting)
        return args[0].get_cost()

SIMPLE_FUNCTIONS = {
    'str'                          : Str,
    'length'                       : Length,
    'parent'                       : Parent,
    ('units', position.Position)   : UnitsPosition,
    ('cost', position.Position)    : CostPosition,
    'year'                         : Year,
    'month'                        : Month,
    'day'                          : Day,
    'weekday'                      : Weekday,
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

class SumBase(c.EvalAggregator):

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = inventory.Inventory()

    def finalize(self, store):
        return store[self.handle]

class SumAmount(SumBase):
    __intypes__ = [amount.Amount]

    def update(self, store, posting):
        value = self.eval_args(posting)[0]
        store[self.handle].add_amount(value)

class SumPosition(SumBase):
    __intypes__ = [position.Position]

    def update(self, store, posting):
        value = self.eval_args(posting)[0]
        store[self.handle].add_position(value)

class SumInventory(SumBase):
    __intypes__ = [inventory.Inventory]

    def update(self, store, posting):
        value = self.eval_args(posting)[0]
        store[self.handle].add_inventory(value)

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
    ('sum', amount.Amount)       : SumAmount,
    ('sum', position.Position)   : SumPosition,
    ('sum', inventory.Inventory) : SumInventory,
    'sum'                        : Sum,
    'count'                      : Count,
    'first'                      : First,
    'last'                       : Last,
    'min'                        : Min,
    'max'                        : Max,
    }




# Column accessors for entries.

class IdEntryColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return hash_entry(entry)

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




class MatchAccount(c.EvalFunction):
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, bool)

    def __call__(self, entry):
        if not isinstance(entry, Transaction):
            return False
        pattern = self.eval_args(entry)[0]
        search = re.compile(pattern, re.IGNORECASE).search
        return any(search(posting.account) for posting in entry.postings)

# Functions defined only on entries.
ENTRY_FUNCTIONS = {
    'has_account' : MatchAccount,
    }


class FilterEntriesEnvironment(c.CompilationEnvironment):
    """An execution context that provides access to attributes on Transactions
    and other entry types.
    """
    context_name = 'FROM clause'
    columns = {
        'id'        : IdEntryColumn,
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
    functions = copy.copy(SIMPLE_FUNCTIONS)
    functions.update(ENTRY_FUNCTIONS)




# Column accessors for postings.

class IdColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return hash_entry(posting.entry)

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
        return posting.entry.tags or EMPTY_SET

class LinksColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(set)

    def __call__(self, posting):
        return posting.entry.links or EMPTY_SET

class PostingFlagColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.flag

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

class CostNumberColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(Decimal)

    def __call__(self, posting):
        return posting.position.lot.cost.number if posting.position.lot.cost else ZERO

class CostCurrencyColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(str)

    def __call__(self, posting):
        return posting.position.lot.cost.currency if posting.position.lot.cost else ''

class ChangeColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(position.Position)

    def __call__(self, posting):
        return posting.position

class PriceColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(amount.Amount)

    def __call__(self, posting):
        return posting.price

class WeightColumn(c.EvalColumn):
    def __init__(self):
        super().__init__(amount.Amount)

    def __call__(self, posting):
        return complete.get_balance_amount(posting)

class FilterPostingsEnvironment(c.CompilationEnvironment):
    """An execution context that provides access to attributes on Postings.
    """
    context_name = 'WHERE clause'
    columns = {
        'id'            : IdColumn,
        'type'          : TypeColumn,
        'filename'      : FilenameColumn,
        'lineno'        : LineNoColumn,
        'date'          : DateColumn,
        'year'          : YearColumn,
        'month'         : MonthColumn,
        'day'           : DayColumn,
        'flag'          : FlagColumn,
        'payee'         : PayeeColumn,
        'narration'     : NarrationColumn,
        'tags'          : TagsColumn,
        'links'         : LinksColumn,
        'posting_flag'  : PostingFlagColumn,
        'account'       : AccountColumn,
        'number'        : NumberColumn,
        'currency'      : CurrencyColumn,
        'cost_number'   : CostNumberColumn,
        'cost_currency' : CostCurrencyColumn,
        'change'        : ChangeColumn,
        'price'         : PriceColumn,
        'weight'        : WeightColumn,
        }
    functions = copy.copy(SIMPLE_FUNCTIONS)

class TargetsEnvironment(FilterPostingsEnvironment):
    """An execution context that provides access to attributes on Postings.
    """
    context_name = 'targets/column'
    functions = copy.copy(FilterPostingsEnvironment.functions)
    functions.update(AGGREGATOR_FUNCTIONS)

    # The list of columns that a wildcard will expand into.
    wildcard_columns = 'date flag payee narration change'.split()
