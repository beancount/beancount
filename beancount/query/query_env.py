"""Environment object for compiler.

This module contains the various column accessors and function evaluators that
are made available by the query compiler via their compilation context objects.
Define new columns and functions here.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import copy
import datetime
import decimal
import re
import textwrap

from beancount.core.number import Decimal
from beancount.core.number import ZERO
from beancount.core.data import Transaction
from beancount.core.compare import hash_entry
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.core import account
from beancount.core import account_types
from beancount.core import data
from beancount.core import getters
from beancount.core import convert
from beancount.core import prices
from beancount.query import query_compile


# Non-agreggating functions. These functionals maintain no state.

class _Neg(query_compile.EvalFunction):
    "Compute the negative value of the argument. This works on various types."
    __intypes__ = None

    def __init__(self, operands):
        super().__init__(operands, self.__intypes__[0])

    def __call__(self, context):
        args = self.eval_args(context)
        return -args[0]

class NegDecimal(_Neg):
    __intypes__ = [Decimal]

class NegAmount(_Neg):
    __intypes__ = [amount.Amount]

class NegPosition(_Neg):
    __intypes__ = [position.Position]

class NegInventory(_Neg):
    __intypes__ = [inventory.Inventory]


class AbsDecimal(query_compile.EvalFunction):
    "Compute the length of the argument. This works on sequences."
    __intypes__ = [Decimal]

    def __init__(self, operands):
        super().__init__(operands, Decimal)

    def __call__(self, context):
        args = self.eval_args(context)
        return abs(args[0])

class AbsPosition(query_compile.EvalFunction):
    "Compute the length of the argument. This works on sequences."
    __intypes__ = [position.Position]

    def __init__(self, operands):
        super().__init__(operands, position.Position)

    def __call__(self, context):
        args = self.eval_args(context)
        return abs(args[0])

class AbsInventory(query_compile.EvalFunction):
    "Compute the length of the argument. This works on sequences."
    __intypes__ = [inventory.Inventory]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        return abs(args[0])

class SafeDiv(query_compile.EvalFunction):
    "A division operation that swallows dbz exceptions and outputs 0 instead."
    __intypes__ = [Decimal, Decimal]

    def __init__(self, operands):
        super().__init__(operands, Decimal)

    def __call__(self, context):
        args = self.eval_args(context)
        try:
            return args[0] / args[1]
        except (decimal.DivisionByZero, decimal.InvalidOperation):
            return ZERO

class SafeDivInt(SafeDiv):
    __intypes__ = [Decimal, int]

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

class YearMonth(query_compile.EvalFunction):
    "Extract the year and month from a date."
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, datetime.date)

    def __call__(self, context):
        args = self.eval_args(context)
        date = args[0]
        return datetime.date(date.year, date.month, 1)

class Quarter(query_compile.EvalFunction):
    "Extract the quarter from a date."
    __intypes__ = [datetime.date]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        date = args[0]
        return '{:04d}-Q{:1d}'.format(date.year, (date.month-1)//3+1)

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

class Today(query_compile.EvalFunction):
    "Today's date"
    __intypes__ = []

    def __init__(self, operands):
        super().__init__(operands, datetime.date)

    def __call__(self, context):
        args = self.eval_args(context)
        return datetime.date.today()


# Operations on accounts.

class Root(query_compile.EvalFunction):
    "Get the root name(s) of the account."
    __intypes__ = [str, int]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        return account.root(args[1], args[0])

class Parent(query_compile.EvalFunction):
    "Get the parent name of the account."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        return account.parent(args[0])

class Leaf(query_compile.EvalFunction):
    "Get the name of the leaf subaccount."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        return account.leaf(args[0])

class Grep(query_compile.EvalFunction):
    "Match a group against a string and return only the matched portion."
    __intypes__ = [str, str]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        match = re.search(args[0], args[1])
        if match:
            return match.group(0)

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

class Meta(query_compile.EvalFunction):
    "Get some metadata key of the Posting."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, object)

    def __call__(self, context):
        args = self.eval_args(context)
        meta = context.posting.meta
        if meta is None:
            return None
        return meta.get(args[0], None)

class EntryMeta(query_compile.EvalFunction):
    "Get some metadata key of the parent directive (Transaction)."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, object)

    def __call__(self, context):
        args = self.eval_args(context)
        meta = context.entry.meta
        if meta is None:
            return None
        return meta.get(args[0], None)

class AnyMeta(query_compile.EvalFunction):
    "Get metadata from the posting or its parent transaction's metadata if not present."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, object)

    def __call__(self, context):
        args = self.eval_args(context)
        key = args[0]

        # Note: if the looked up key is explicitly defined in posting as None,
        # we return it, rather than falling back to parent Transaction.
        posting_meta = context.posting.meta
        entry_meta = context.entry.meta
        if posting_meta and key in posting_meta:
            value = posting_meta[key]
        elif entry_meta and key in entry_meta:
            value = entry_meta[key]
        else:
            value = None
        return value

class OpenMeta(query_compile.EvalFunction):
    "Get the metadata dict of the open directive of the account."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, dict)

    def __call__(self, context):
        args = self.eval_args(context)
        open_entry, _ = context.open_close_map[args[0]]
        return open_entry.meta

class AccountSortKey(query_compile.EvalFunction):
    "Get a string to sort accounts in order taking into account the types."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        index, name = account_types.get_account_sort_key(context.account_types, args[0])
        return '{}-{}'.format(index, name)

# Note: Don't provide this, because polymorphic multiplication on Amount,
# Position, Inventory isn't supported yet.
#
# class AccountSign(query_compile.EvalFunction):
#     "Produce a +1 / -1 signed value to multiply with to correct balances."
#     __intypes__ = [str]
#
#     def __init__(self, operands):
#         super().__init__(operands, Decimal)
#
#     def __call__(self, context):
#         args = self.eval_args(context)
#         return Decimal(account_types.get_account_sign(args[0], context.account_types))

class CurrencyMeta(query_compile.EvalFunction):
    "Get the metadata dict of the commodity directive of the currency."
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, dict)

    def __call__(self, context):
        args = self.eval_args(context)
        commodity_entry = context.commodity_map[args[0]]
        return commodity_entry.meta


# Operation on inventories, positions and amounts.

class UnitsPosition(query_compile.EvalFunction):
    "Get the number of units of a position (stripping cost)."
    __intypes__ = [position.Position]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].units

class UnitsInventory(query_compile.EvalFunction):
    "Get the number of units of an inventory (stripping cost)."
    __intypes__ = [inventory.Inventory]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].reduce(convert.get_units)

class CostPosition(query_compile.EvalFunction):
    "Get the cost of a position."
    __intypes__ = [position.Position]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        return convert.get_cost(args[0])

class CostInventory(query_compile.EvalFunction):
    "Get the cost of an inventory."
    __intypes__ = [inventory.Inventory]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].reduce(convert.get_cost)


class ConvertAmount(query_compile.EvalFunction):
    "Coerce an amount to a particular currency."
    __intypes__ = [amount.Amount, str]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        amount_, currency = args
        return convert.convert_amount(amount_, currency, context.price_map, None)

class ConvertAmountWithDate(query_compile.EvalFunction):
    "Coerce an amount to a particular currency."
    __intypes__ = [amount.Amount, str, datetime.date]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        amount_, currency, date = args
        return convert.convert_amount(amount_, currency, context.price_map, date)


class ConvertPosition(query_compile.EvalFunction):
    "Coerce an amount to a particular currency."
    __intypes__ = [position.Position, str]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        pos, currency = args
        return convert.convert_position(pos, currency, context.price_map, None)

class ConvertPositionWithDate(query_compile.EvalFunction):
    "Coerce an amount to a particular currency."
    __intypes__ = [position.Position, str, datetime.date]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        pos, currency, date = args
        return convert.convert_position(pos, currency, context.price_map, date)


class ValuePosition(query_compile.EvalFunction):
    "Convert a position to its cost currency at the market value."
    __intypes__ = [position.Position]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        pos = args[0]
        return convert.get_value(pos, context.price_map, None)

class ValuePositionWithDate(query_compile.EvalFunction):
    "Convert a position to its cost currency at the market value of a particular date."
    __intypes__ = [position.Position, datetime.date]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        pos, date = args
        return convert.get_value(pos, context.price_map, date)


class ConvertInventory(query_compile.EvalFunction):
    "Coerce an inventory to a particular currency."
    __intypes__ = [inventory.Inventory, str]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        inv, currency = args
        return inv.reduce(convert.convert_position, currency, context.price_map, None)

class ConvertInventoryWithDate(query_compile.EvalFunction):
    "Coerce an inventory to a particular currency."
    __intypes__ = [inventory.Inventory, str, datetime.date]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        inv, currency, date = args
        return inv.reduce(convert.convert_position, currency, context.price_map, date)


class ValueInventory(query_compile.EvalFunction):
    "Coerce an inventory to its market value at the current date."
    __intypes__ = [inventory.Inventory]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        inv = args[0]
        return inv.reduce(convert.get_value, context.price_map, None)

class ValueInventoryWithDate(query_compile.EvalFunction):
    "Coerce an inventory to its market value at a particular date."
    __intypes__ = [inventory.Inventory, datetime.date]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        inv, date = args
        return inv.reduce(convert.get_value, context.price_map, date)


class Price(query_compile.EvalFunction):
    "Fetch a price for something at a particular date"
    __intypes__ = [str, str]

    def __init__(self, operands):
        super().__init__(operands, Decimal)

    def __call__(self, context):
        args = self.eval_args(context)
        base, quote = args
        pair = (base.upper(), quote.upper())
        _, price = prices.get_price(context.price_map, pair, None)
        return price

class PriceWithDate(query_compile.EvalFunction):
    "Fetch a price for something at a particular date"
    __intypes__ = [str, str, datetime.date]

    def __init__(self, operands):
        super().__init__(operands, Decimal)

    def __call__(self, context):
        args = self.eval_args(context)
        base, quote, date = args
        pair = (base.upper(), quote.upper())
        _, price = prices.get_price(context.price_map, pair, date)
        return price


class Number(query_compile.EvalFunction):
    "Extract the number from an Amount."
    __intypes__ = [amount.Amount]

    def __init__(self, operands):
        super().__init__(operands, Decimal)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].number

class Currency(query_compile.EvalFunction):
    "Extract the currency from an Amount."
    __intypes__ = [amount.Amount]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        return args[0].currency

class GetItemStr(query_compile.EvalFunction):
    "Get the string value of a dict. The value is always converted to a string."
    __intypes__ = [dict, str]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        value = args[0].get(args[1])
        if value is None:
            value = ''
        elif not isinstance(value, str):
            value = str(value)
        return value

class FindFirst(query_compile.EvalFunction):
    "Filter a string sequence by regular expression and return the first match."
    __intypes__ = [str, set]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        values = args[1]
        if not values:
            return
        for value in sorted(values):
            if re.match(args[0], value):
                return value

class JoinStr(query_compile.EvalFunction):
    "Join a sequence of strings to a single comma-separated string."
    __intypes__ = [set]

    def __init__(self, operands):
        super().__init__(operands, str)

    def __call__(self, context):
        args = self.eval_args(context)
        values = args[0]
        return ','.join(values)


class OnlyInventory(query_compile.EvalFunction):
    "Get one currency's amount from the inventory."
    __intypes__ = [str, inventory.Inventory]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        currency, inventory_ = self.eval_args(context)
        return inventory_.get_currency_units(currency)

class FilterCurrencyPosition(query_compile.EvalFunction):
    "Filter an inventory to just the specified currency."
    __intypes__ = [position.Position, str]

    def __init__(self, operands):
        super().__init__(operands, position.Position)

    def __call__(self, context):
        args = self.eval_args(context)
        pos, currency = args
        return pos if pos.units.currency == currency else None

class FilterCurrencyInventory(query_compile.EvalFunction):
    "Filter an inventory to just the specified currency."
    __intypes__ = [inventory.Inventory, str]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        inv, currency = args
        return inventory.Inventory(pos
                                   for pos in inv
                                   if pos.units.currency == currency)


class PosSignDecimal(query_compile.EvalFunction):
    "Correct sign of an Amount based on the usual balance of associated account."
    __intypes__ = [Decimal, str]

    def __init__(self, operands):
        super().__init__(operands, Decimal)

    def __call__(self, context):
        args = self.eval_args(context)
        num, account = args
        sign = account_types.get_account_sign(account, context.account_types)
        return num if sign >= 0  else -num

class PosSignAmount(query_compile.EvalFunction):
    "Correct sign of an Amount based on the usual balance of associated account."
    __intypes__ = [amount.Amount, str]

    def __init__(self, operands):
        super().__init__(operands, amount.Amount)

    def __call__(self, context):
        args = self.eval_args(context)
        amt, account = args
        sign = account_types.get_account_sign(account, context.account_types)
        return amt if sign >= 0  else -amt

class PosSignPosition(query_compile.EvalFunction):
    "Correct sign of an Amount based on the usual balance of associated account."
    __intypes__ = [position.Position, str]

    def __init__(self, operands):
        super().__init__(operands, position.Position)

    def __call__(self, context):
        args = self.eval_args(context)
        pos, account = args
        sign = account_types.get_account_sign(account, context.account_types)
        return pos if sign >= 0  else -pos

class PosSignInventory(query_compile.EvalFunction):
    "Correct sign of an Amount based on the usual balance of associated account."
    __intypes__ = [inventory.Inventory, str]

    def __init__(self, operands):
        super().__init__(operands, inventory.Inventory)

    def __call__(self, context):
        args = self.eval_args(context)
        inv, account = args
        sign = account_types.get_account_sign(account, context.account_types)
        return inv if sign >= 0  else -inv



# FIXME: Why do I need to specify the arguments here? They are already derived
# from the functions. Just fetch them from instead. Make the compiler better.
SIMPLE_FUNCTIONS = {
    ('neg', Decimal)                                     : NegDecimal,
    ('neg', amount.Amount)                               : NegAmount,
    ('neg', position.Position)                           : NegPosition,
    ('neg', inventory.Inventory)                         : NegInventory,
    ('abs', Decimal)                                     : AbsDecimal,
    ('abs', position.Position)                           : AbsPosition,
    ('abs', inventory.Inventory)                         : AbsInventory,
    ('safediv', Decimal, Decimal)                        : SafeDiv,
    ('safediv', Decimal, int)                            : SafeDivInt,
    'length'                                             : Length,
    'str'                                                : Str,
    'maxwidth'                                           : MaxWidth,
    'root'                                               : Root,
    'parent'                                             : Parent,
    'leaf'                                               : Leaf,
    'grep'                                               : Grep,
    'open_date'                                          : OpenDate,
    'close_date'                                         : CloseDate,
    'meta'                                               : Meta,
    'entry_meta'                                         : EntryMeta,
    'any_meta'                                           : AnyMeta,
    'open_meta'                                          : OpenMeta,
    'currency_meta'                                      : CurrencyMeta,
    'commodity_meta'                                     : CurrencyMeta,  # Redundant.
    'account_sortkey'                                    : AccountSortKey,
    ('units', position.Position)                         : UnitsPosition,
    ('units', inventory.Inventory)                       : UnitsInventory,
    ('cost', position.Position)                          : CostPosition,
    ('cost', inventory.Inventory)                        : CostInventory,
    'year'                                               : Year,
    'month'                                              : Month,
    'ymonth'                                             : YearMonth,
    'quarter'                                            : Quarter,
    'day'                                                : Day,
    'weekday'                                            : Weekday,
    'today'                                              : Today,
    ('convert', amount.Amount, str)                      : ConvertAmount,
    ('convert', amount.Amount, str, datetime.date)       : ConvertAmountWithDate,
    ('convert', position.Position, str)                  : ConvertPosition,
    ('convert', position.Position, str, datetime.date)   : ConvertPositionWithDate,
    ('convert', inventory.Inventory, str)                : ConvertInventory,
    ('convert', inventory.Inventory, str, datetime.date) : ConvertInventoryWithDate,
    ('value', position.Position)                         : ValuePosition,
    ('value', position.Position, datetime.date)          : ValuePositionWithDate,
    ('value', inventory.Inventory)                       : ValueInventory,
    ('value', inventory.Inventory, datetime.date)        : ValueInventoryWithDate,
    ('getprice', str, str)                               : Price,
    ('getprice', str, str, datetime.date)                : PriceWithDate,
    'number'                                             : Number,
    'currency'                                           : Currency,
    'commodity'                                          : Currency,  # Redundant.
    'getitem'                                            : GetItemStr,
    'findfirst'                                          : FindFirst,
    'joinstr'                                            : JoinStr,
    ('possign', Decimal, str)                            : PosSignDecimal,
    ('possign', amount.Amount, str)                      : PosSignAmount,
    ('possign', position.Position, str)                  : PosSignPosition,
    ('possign', inventory.Inventory, str)                : PosSignInventory,

    # FIXME: 'only' should be removed.
    'only'                                               : OnlyInventory,
    ('filter_currency', position.Position, str)          : FilterCurrencyPosition,
    ('filter_currency', inventory.Inventory, str)        : FilterCurrencyInventory,
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
        if value is not None:
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

    def __call__(self, context):
        return hash_entry(context.entry)

class TypeEntryColumn(query_compile.EvalColumn):
    "The data type of the directive."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return type(context.entry).__name__.lower()

class FilenameEntryColumn(query_compile.EvalColumn):
    "The filename where the directive was parsed from or created."
    __equivalent__ = 'entry.meta["filename"]'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.entry.meta["filename"]

class LineNoEntryColumn(query_compile.EvalColumn):
    "The line number from the file the directive was parsed from."
    __equivalent__ = 'entry.meta["lineno"]'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.entry.meta["lineno"]

class DateEntryColumn(query_compile.EvalColumn):
    "The date of the directive."
    __equivalent__ = 'entry.date'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(datetime.date)

    def __call__(self, context):
        return context.entry.date

class YearEntryColumn(query_compile.EvalColumn):
    "The year of the date of the directive."
    __equivalent__ = 'entry.date.year'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.entry.date.year

class MonthEntryColumn(query_compile.EvalColumn):
    "The month of the date of the directive."
    __equivalent__ = 'entry.date.month'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.entry.date.month

class DayEntryColumn(query_compile.EvalColumn):
    "The day of the date of the directive."
    __equivalent__ = 'entry.date.day'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.entry.date.day

class FlagEntryColumn(query_compile.EvalColumn):
    "The flag the transaction."
    __equivalent__ = 'entry.flag'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return (context.entry.flag
                if isinstance(context.entry, Transaction)
                else None)

class PayeeEntryColumn(query_compile.EvalColumn):
    "The payee of the transaction."
    __equivalent__ = 'entry.payee'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return (context.entry.payee or ''
                if isinstance(context.entry, Transaction)
                else None)

class NarrationEntryColumn(query_compile.EvalColumn):
    "The narration of the transaction."
    __equivalent__ = 'entry.narration'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return (context.entry.narration or ''
                if isinstance(context.entry, Transaction)
                else None)

# This is convenient, because many times the payee is empty and using a
# combination produces more compact listings.
class DescriptionEntryColumn(query_compile.EvalColumn):
    "A combination of the payee + narration of the transaction, if present."
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return (' | '.join(filter(None, [context.entry.payee,
                                         context.entry.narration]))
                if isinstance(entry, Transaction)
                else None)


# A globally available empty set to fill in for None's.
EMPTY_SET = frozenset()

class TagsEntryColumn(query_compile.EvalColumn):
    "The set of tags of the transaction."
    __equivalent__ = 'entry.tags'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(set)

    def __call__(self, context):
        return (context.entry.tags or EMPTY_SET
                if isinstance(context.entry, Transaction)
                else EMPTY_SET)

class LinksEntryColumn(query_compile.EvalColumn):
    "The set of links of the transaction."
    __equivalent__ = 'entry.links'
    __intypes__ = [data.Transaction]

    def __init__(self):
        super().__init__(set)

    def __call__(self, context):
        return (context.entry.links or EMPTY_SET
                if isinstance(context.entry, Transaction)
                else EMPTY_SET)



class MatchAccount(query_compile.EvalFunction):
    """A predicate, true if the transaction has at least one posting matching
    the regular expression argument."""
    __intypes__ = [str]

    def __init__(self, operands):
        super().__init__(operands, bool)

    def __call__(self, context):
        pattern = self.eval_args(context)[0]
        search = re.compile(pattern, re.IGNORECASE).search
        return any(search(account) for account in getters.get_entry_accounts(context.entry))


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
        'id'          : IdEntryColumn,
        'type'        : TypeEntryColumn,
        'filename'    : FilenameEntryColumn,
        'lineno'      : LineNoEntryColumn,
        'date'        : DateEntryColumn,
        'year'        : YearEntryColumn,
        'month'       : MonthEntryColumn,
        'day'         : DayEntryColumn,
        'flag'        : FlagEntryColumn,
        'payee'       : PayeeEntryColumn,
        'narration'   : NarrationEntryColumn,
        'description' : DescriptionEntryColumn,
        'tags'        : TagsEntryColumn,
        'links'       : LinksEntryColumn,
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
        return hash_entry(context.entry)

class TypeColumn(query_compile.EvalColumn):
    "The data type of the parent transaction for this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return type(context.entry).__name__.lower()

class FilenameColumn(query_compile.EvalColumn):
    "The filename where the posting was parsed from or created."
    __equivalent__ = 'entry.meta["filename"]'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.entry.meta["filename"]

class LineNoColumn(query_compile.EvalColumn):
    "The line number from the file the posting was parsed from."
    __equivalent__ = 'entry.meta["lineno"]'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.entry.meta["lineno"]

class FileLocationColumn(query_compile.EvalColumn):
    """The filename:lineno where the posting was parsed from or created.

    If you select this column as the first column, because it renders like
    errors, Emacs is able to pick those up and you can navigate between an
    arbitrary list of transactions with next-error and previous-error.
    """
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        if context.posting.meta is not None:
            return '{}:{:d}:'.format(context.posting.meta.get("filename", "N/A"),
                                     context.posting.meta.get("lineno", 0))
        else:
            return '' # Unknown.

class DateColumn(query_compile.EvalColumn):
    "The date of the parent transaction for this posting."
    __equivalent__ = 'entry.date'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(datetime.date)

    def __call__(self, context):
        return context.entry.date

class YearColumn(query_compile.EvalColumn):
    "The year of the date of the parent transaction for this posting."
    __equivalent__ = 'entry.date.year'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.entry.date.year

class MonthColumn(query_compile.EvalColumn):
    "The month of the date of the parent transaction for this posting."
    __equivalent__ = 'entry.date.month'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.entry.date.month

class DayColumn(query_compile.EvalColumn):
    "The day of the date of the parent transaction for this posting."
    __equivalent__ = 'entry.date.day'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(int)

    def __call__(self, context):
        return context.entry.date.day

class FlagColumn(query_compile.EvalColumn):
    "The flag of the parent transaction for this posting."
    __equivalent__ = 'entry.flag'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.entry.flag

class PayeeColumn(query_compile.EvalColumn):
    "The payee of the parent transaction for this posting."
    __equivalent__ = 'entry.payee'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.entry.payee or ''

class NarrationColumn(query_compile.EvalColumn):
    "The narration of the parent transaction for this posting."
    __equivalent__ = 'entry.narration'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.entry.narration

# This is convenient, because many times the payee is empty and using a
# combination produces more compact listings.
class DescriptionColumn(query_compile.EvalColumn):
    "A combination of the payee + narration for the transaction of this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        entry = context.entry
        return (' | '.join(filter(None, [entry.payee, entry.narration]))
                if isinstance(entry, Transaction)
                else None)

class TagsColumn(query_compile.EvalColumn):
    "The set of tags of the parent transaction for this posting."
    __equivalent__ = 'entry.tags'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(set)

    def __call__(self, context):
        return context.entry.tags or EMPTY_SET

class LinksColumn(query_compile.EvalColumn):
    "The set of links of the parent transaction for this posting."
    __equivalent__ = 'entry.links'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(set)

    def __call__(self, context):
        return context.entry.links or EMPTY_SET

class PostingFlagColumn(query_compile.EvalColumn):
    "The flag of the posting itself."
    __equivalent__ = 'posting.flag'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.flag

class AccountColumn(query_compile.EvalColumn):
    "The account of the posting."
    __equivalent__ = 'posting.account'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.account

class OtherAccountsColumn(query_compile.EvalColumn):
    "The list of other accounts in the transcation, excluding that of this posting."
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(set)

    def __call__(self, context):
        return sorted({posting.account
                       for posting in context.entry.postings
                       if posting is not context.posting})


class NumberColumn(query_compile.EvalColumn):
    "The number of units of the posting."
    __equivalent__ = 'posting.units.number'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(Decimal)

    def __call__(self, context):
        return context.posting.units.number

class CurrencyColumn(query_compile.EvalColumn):
    "The currency of the posting."
    __equivalent__ = 'posting.units.currency'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        return context.posting.units.currency

class CostNumberColumn(query_compile.EvalColumn):
    "The number of cost units of the posting."
    __equivalent__ = 'posting.cost.number'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(Decimal)

    def __call__(self, context):
        cost = context.posting.cost
        return cost.number if cost else None

class CostCurrencyColumn(query_compile.EvalColumn):
    "The cost currency of the posting."
    __equivalent__ = 'posting.cost.currency'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        cost = context.posting.cost
        return cost.currency if cost else ''

class CostDateColumn(query_compile.EvalColumn):
    "The cost currency of the posting."
    __equivalent__ = 'posting.cost.date'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(datetime.date)

    def __call__(self, context):
        cost = context.posting.cost
        return cost.date if cost else None

class CostLabelColumn(query_compile.EvalColumn):
    "The cost currency of the posting."
    __equivalent__ = 'posting.cost.label'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(str)

    def __call__(self, context):
        cost = context.posting.cost
        return cost.label if cost else ''

class PositionColumn(query_compile.EvalColumn):
    "The position for the posting. These can be summed into inventories."
    __equivalent__ = 'posting'
    __intypes__ = [data.Posting]

    def __init__(self):
        super().__init__(position.Position)

    def __call__(self, context):
        posting = context.posting
        return position.Position(posting.units, posting.cost)

class PriceColumn(query_compile.EvalColumn):
    "The price attached to the posting."
    __equivalent__ = 'posting.price'
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
        return convert.get_weight(context.posting)

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
        'id'             : IdColumn,
        'type'           : TypeColumn,
        'filename'       : FilenameColumn,
        'lineno'         : LineNoColumn,
        'location'       : FileLocationColumn,
        'date'           : DateColumn,
        'year'           : YearColumn,
        'month'          : MonthColumn,
        'day'            : DayColumn,
        'flag'           : FlagColumn,
        'payee'          : PayeeColumn,
        'narration'      : NarrationColumn,
        'description'    : DescriptionColumn,
        'tags'           : TagsColumn,
        'links'          : LinksColumn,
        'posting_flag'   : PostingFlagColumn,
        'account'        : AccountColumn,
        'other_accounts' : OtherAccountsColumn,
        'number'         : NumberColumn,
        'currency'       : CurrencyColumn,
        'cost_number'    : CostNumberColumn,
        'cost_currency'  : CostCurrencyColumn,
        'cost_date'      : CostDateColumn,
        'cost_label'     : CostLabelColumn,
        'position'       : PositionColumn,
        'change'         : PositionColumn,  # Backwards compatible.
        'price'          : PriceColumn,
        'weight'         : WeightColumn,
        'balance'        : BalanceColumn,
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
