"""Environment object for compiler.

This module contains the various column accessors and function evaluators that
are made available by the query compiler via their compilation context objects.
Define new columns and functions here.
"""
import copy
import datetime
import re
import textwrap

from beancount.core.amount import Decimal
from beancount.core.amount import ZERO
from beancount.core.data import Transaction
from beancount.core.compare import hash_entry
from beancount.core import interpolate
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.core import account
from beancount.core import account_types
from beancount.core import data
from beancount.core import getters
from beancount.query import query_compile


# Non-agreggating functions. These functionals maintain no state.

class Length(query_compile.EvalFunction):
    "Compute the length of the argument. This works on sequences."
    __intypes__ = [(list, set, str)]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, context):
        args = self.eval_args(context)
        return len(args[0])

class Str(query_compile.EvalFunction):
    "Convert the argument to a string."
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        return repr(args[0])

class MaxWidth(query_compile.EvalFunction):
    "Convert the argument to a substring. This can be used to ensure maximum width"
    __intypes__ = [str, int]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        string, width = self.eval_args(context)
        return textwrap.shorten(string, width=width)


# Operations on dates.

class Year(query_compile.EvalFunction):
    "Extract the year from a date."
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].year

class Month(query_compile.EvalFunction):
    "Extract the month from a date."
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].month

class Day(query_compile.EvalFunction):
    "Extract the day from a date."
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, int)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].day

class Weekday(query_compile.EvalFunction):
    "Extract a 3-letter weekday from a date."
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].strftime('%a')


# Operations on accounts.

class Parent(query_compile.EvalFunction):
    "Get the parent name of the account."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        return account.parent(args[0])

class OpenDate(query_compile.EvalFunction):
    "Get the date of the open directive of the account."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, datetime.date)

    def __call__(self, context):
        args = self.eval_args(context)
        open_entry, close_entry = context.open_close_map[args[0]]
        return open_entry.date if open_entry else None

class CloseDate(query_compile.EvalFunction):
    "Get the date of the close directive of the account."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, datetime.date)

    def __call__(self, context):
        args = self.eval_args(context)
        close_entry, close_entry = context.open_close_map[args[0]]
        return close_entry.date if close_entry else None

class AccountSortKey(query_compile.EvalFunction):
    "Get a string to sort accounts in order taking into account the types."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        index, name = account_types.get_account_sort_key(context.account_types, args[0])
        return '{}-{}'.format(index, name)


# Operation on inventories.

class UnitsPosition(query_compile.EvalFunction):
    "Get the number of units of a position (stripping cost)."
    __intypes__ = [position.Position]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].get_units()

class UnitsInventory(query_compile.EvalFunction):
    "Get the number of units of a position (stripping cost)."
    __intypes__ = [inventory.Inventory]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].units()

class CostPosition(query_compile.EvalFunction):
    "Get the cost of a position."
    __intypes__ = [position.Position]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].get_cost()

class CostInventory(query_compile.EvalFunction):
    "Get the cost of an inventory."
    __intypes__ = [inventory.Inventory]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].cost()

SIMPLE_FUNCTIONS = {
    'str'                          : Str,
    'length'                       : Length,
    'maxwidth'                     : MaxWidth,
    'parent'                       : Parent,
    'open_date'                    : OpenDate,
    'close_date'                   : CloseDate,
    'account_sortkey'              : AccountSortKey,
    ('units', position.Position)   : UnitsPosition,
    ('units', inventory.Inventory) : UnitsInventory,
    ('cost', position.Position)    : CostPosition,
    ('cost', inventory.Inventory)  : CostInventory,
    'year'                         : Year,
    'month'                        : Month,
    'day'                          : Day,
    'weekday'                      : Weekday,
    }



# Aggregating functions. These instances themselves both make the computation
# and manage state for a single iteration.

class Count(query_compile.EvalAggregator):
    "Count the number of occurrences of the argument."
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, int)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = 0

    def update(self, store, unused_ontext):
        store[self.handle] += 1

    def __call__(self, context):
        return context.store[self.handle]

class Sum(query_compile.EvalAggregator):
    "Calculate the sum of the numerical argument."
    __intypes__ = [(int, float, Decimal)]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = self.dtype()

    def update(self, store, context):
        value = self.eval_args(context)[0]
        store[self.handle] += value

    def __call__(self, context):
        return context.store[self.handle]

class SumBase(query_compile.EvalAggregator):

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = inventory.Inventory()

    def __call__(self, context):
        return context.store[self.handle]

class SumAmount(SumBase):

    "Calculate the sum of the amount. The result is an Inventory."
    __intypes__ = [amount.Amount]

    def update(self, store, context):
        value = self.eval_args(context)[0]
        store[self.handle].add_amount(value)

class SumPosition(SumBase):
    "Calculate the sum of the position. The result is an Inventory."
    __intypes__ = [position.Position]

    def update(self, store, context):
        value = self.eval_args(context)[0]
        store[self.handle].add_position(value)

class SumInventory(SumBase):
    "Calculate the sum of the inventories. The result is an Inventory."
    __intypes__ = [inventory.Inventory]

    def update(self, store, context):
        value = self.eval_args(context)[0]
        store[self.handle].add_inventory(value)

class First(query_compile.EvalAggregator):
    "Keep the first of the values seen."
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = None

    def update(self, store, context):
        if store[self.handle] is None:
            value = self.eval_args(context)[0]
            store[self.handle] = value

    def __call__(self, context):
        return context.store[self.handle]

class Last(query_compile.EvalAggregator):
    "Keep the last of the values seen."
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = None

    def update(self, store, context):
        value = self.eval_args(context)[0]
        store[self.handle] = value

    def __call__(self, context):
        return context.store[self.handle]

class Min(query_compile.EvalAggregator):
    "Compute the minimum of the values."
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = self.dtype()

    def update(self, store, context):
        value = self.eval_args(context)[0]
        if value < store[self.handle]:
            store[self.handle] = value

    def __call__(self, context):
        return context.store[self.handle]

class Max(query_compile.EvalAggregator):
    "Compute the maximum of the values."
    __intypes__ = [object]

    def __init__(self, operands):
        super().__init__(operands, operands[0].dtype)

    def allocate(self, allocator):
        self.handle = allocator.allocate()

    def initialize(self, store):
        store[self.handle] = self.dtype()

    def update(self, store, context):
        value = self.eval_args(context)[0]
        if value > store[self.handle]:
            store[self.handle] = value

    def __call__(self, context):
        return context.store[self.handle]

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

class IdEntryColumn(query_compile.EvalColumn):
    "Unique id of a directive."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return hash_entry(entry)

class TypeEntryColumn(query_compile.EvalColumn):
    "The data type of the directive."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return type(entry).__name__.lower()

class FilenameEntryColumn(query_compile.EvalColumn):
    "The filename where the directive was parsed from or created."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return entry.source.filename

class LineNoEntryColumn(query_compile.EvalColumn):
    "The line number from the file the directive was parsed from."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(int)

    def __call__(self, entry):
        return entry.source.lineno

class DateEntryColumn(query_compile.EvalColumn):
    "The date of the directive."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(datetime.date)

    def __call__(self, entry):
        return entry.date

class YearEntryColumn(query_compile.EvalColumn):
    "The year of the date of the directive."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(int)

    def __call__(self, entry):
        return entry.date.year

class MonthEntryColumn(query_compile.EvalColumn):
    "The month of the date of the directive."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(int)

    def __call__(self, entry):
        return entry.date.month

class DayEntryColumn(query_compile.EvalColumn):
    "The day of the date of the directive."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(int)

    def __call__(self, entry):
        return entry.date.day

class FlagEntryColumn(query_compile.EvalColumn):
    "The flag the transaction."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return (entry.flag
                if isinstance(entry, Transaction)
                else None)

class PayeeEntryColumn(query_compile.EvalColumn):
    "The payee of the transaction."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return (entry.payee or ''
                if isinstance(entry, Transaction)
                else None)

class NarrationEntryColumn(query_compile.EvalColumn):
    "The narration of the transaction."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, entry):
        return (entry.narration or ''
                if isinstance(entry, Transaction)
                else None)

# A globally available empty set to fill in for None's.
EMPTY_SET = frozenset()

class TagsEntryColumn(query_compile.EvalColumn):
    "The set of tags of the transaction."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(set)

    def __call__(self, entry):
        return (entry.tags or EMPTY_SET
                if isinstance(entry, Transaction)
                else EMPTY_SET)

class LinksEntryColumn(query_compile.EvalColumn):
    "The set of links of the transaction."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(set)

    def __call__(self, entry):
        return (entry.links or EMPTY_SET
                if isinstance(entry, Transaction)
                else EMPTY_SET)



class MatchAccount(query_compile.EvalFunction):
    """A predicate, true if the transaction has at least one posting matching
    the regular expression argument."""
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, bool)

    def __call__(self, entry):
        pattern = self.eval_args(entry)[0]
        search = re.compile(pattern, re.IGNORECASE).search
        return any(search(account) for account in getters.get_entry_accounts(entry))


# Functions defined only on entries.
ENTRY_FUNCTIONS = {
    'has_account' : MatchAccount,
    }


class FilterEntriesEnvironment(query_compile.CompilationEnvironment):
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

class IdColumn(query_compile.EvalColumn):
    "The unique id of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return hash_entry(context.posting.entry)

class TypeColumn(query_compile.EvalColumn):
    "The data type of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return type(context.posting.entry).__name__.lower()

class FilenameColumn(query_compile.EvalColumn):
    "The filename where the posting was parsed from or created."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.entry.source.filename

class LineNoColumn(query_compile.EvalColumn):
    "The line number from the file the posting was parsed from."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.posting.entry.source.lineno

class DateColumn(query_compile.EvalColumn):
    "The date of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(datetime.date)

    def __call__(self, context):
        return context.posting.entry.date

class YearColumn(query_compile.EvalColumn):
    "The year of the date of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.posting.entry.date.year

class MonthColumn(query_compile.EvalColumn):
    "The month of the date of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.posting.entry.date.month

class DayColumn(query_compile.EvalColumn):
    "The day of the date of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.posting.entry.date.day

class FlagColumn(query_compile.EvalColumn):
    "The flag of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.entry.flag

class PayeeColumn(query_compile.EvalColumn):
    "The payee of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.entry.payee or ''

class NarrationColumn(query_compile.EvalColumn):
    "The narration of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.entry.narration

class TagsColumn(query_compile.EvalColumn):
    "The set of tags of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(set)

    def __call__(self, context):
        return context.posting.entry.tags or EMPTY_SET

class LinksColumn(query_compile.EvalColumn):
    "The set of links of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(set)

    def __call__(self, context):
        return context.posting.entry.links or EMPTY_SET

class PostingFlagColumn(query_compile.EvalColumn):
    "The flag of the posting itself."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.flag

class AccountColumn(query_compile.EvalColumn):
    "The account of the posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.account

class NumberColumn(query_compile.EvalColumn):
    "The number of units of the posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(Decimal)

    def __call__(self, context):
        return context.posting.position.number

class CurrencyColumn(query_compile.EvalColumn):
    "The currency of the posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.position.lot.currency

class CostNumberColumn(query_compile.EvalColumn):
    "The number of cost units of the posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(Decimal)

    def __call__(self, context):
        cost = context.posting.position.lot.cost
        return cost.number if cost else ZERO

class CostCurrencyColumn(query_compile.EvalColumn):
    "The cost currency of the posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        cost = context.posting.position.lot.cost
        return cost.currency if cost else ''

class PositionColumn(query_compile.EvalColumn):
    "The position for the posting. These can be summed into inventories."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(position.Position)

    def __call__(self, context):
        return context.posting.position

class PriceColumn(query_compile.EvalColumn):
    "The price attached to the posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(amount.Amount)

    def __call__(self, context):
        return context.posting.price

class WeightColumn(query_compile.EvalColumn):
    "The computed weight used for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(amount.Amount)

    def __call__(self, context):
        return interpolate.get_balance_amount(context)

class BalanceColumn(query_compile.EvalColumn):
    "The balance for the posting. These can be summed into inventories."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(inventory.Inventory)

    def __call__(self, context):
        return copy.copy(context.balance)


class FilterPostingsEnvironment(query_compile.CompilationEnvironment):
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
        'position'      : PositionColumn,
        'change'        : PositionColumn,  # Backwards compatible.
        'price'         : PriceColumn,
        'weight'        : WeightColumn,
        'balance'       : BalanceColumn,
        }
    functions = copy.copy(SIMPLE_FUNCTIONS)

class TargetsEnvironment(FilterPostingsEnvironment):
    """An execution context that provides access to attributes on Postings.
    """
    context_name = 'targets/column'
    functions = copy.copy(FilterPostingsEnvironment.functions)
    functions.update(AGGREGATOR_FUNCTIONS)

    # The list of columns that a wildcard will expand into.
    wildcard_columns = 'date flag payee narration position'.split()
