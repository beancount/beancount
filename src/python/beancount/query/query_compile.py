"""Compilation/translation for the query language AST.

This code accepts the abstract syntax tree produced by the query parser and
resolves the column and function names, and prepares execution state to be run
against a list of entries.
"""
import collections
import itertools
import io

import ply.lex
import ply.yacc

from beancount.core import position


class TypeColumn(Column):
    def __call__(self, posting):
        return type(posting.entry)

class FilenameColumn(Column):
    def __call__(self, posting):
        return posting.entry.source.filename

class LineNoColumn(Column):
    def __call__(self, posting):
        return posting.entry.source.lineno

class DateColumn(Column):
    def __call__(self, posting):
        return posting.entry.date

class YearColumn(Column):
    def __call__(self, posting):
        return posting.entry.date.year

class FlagColumn(Column):
    def __call__(self, posting):
        return posting.entry.flag

class PayeeColumn(Column):
    def __call__(self, posting):
        return posting.entry.payee

class NarrationColumn(Column):
    def __call__(self, posting):
        return posting.entry.narration

class TagsColumn(Column):
    def __call__(self, posting):
        return posting.entry.tags

class LinksColumn(Column):
    def __call__(self, posting):
        return posting.entry.links

class AccountColumn(Column):
    def __call__(self, posting):
        return posting.account

class NumberColumn(Column):
    def __call__(self, posting):
        return posting.position.number

class CurrencyColumn(Column):
    def __call__(self, posting):
        return posting.position.lot.currency

class ChangeColumn(Column):
    def __call__(self, posting):
        return posting.position

POSTING_COLUMNS = {
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





class Length(Function):
    def __call__(self, posting):
        child_value = self.child(posting)
        return len(child_value)

class Units(Function):
    def __call__(self, posting):
        position_ = self.child(posting)
        assert isinstance(position_, position.Position)
        return position_.get_amount()

class Cost(Function):
    def __call__(self, posting):
        position_ = self.child(posting)
        assert isinstance(position_, position.Position)
        return position_.get_cost()


class Aggregator(Function):
    def __init__(self, **kwds):
        super().__init__(**kwds)
        self.agg_value = None

class Sum(Aggregator):
    def __call__(self, posting):
        child_value = self.child(posting)
        if self.agg_value is None:
            self.agg_value = child_value
        else:
            self.agg_value += child_value

class First(Aggregator):
    def __call__(self, posting):
        if self.agg_value is None:
            self.agg_value = self.child(posting)

class Last(Aggregator):
    def __call__(self, posting):
        self.agg_value = self.child(posting)


FUNCTIONS = {
    'length': Length,
    'units': Units,
    'cost': Cost,
    'sum': Sum,
    'first': First,
    'last': Last,
    }



class DateEntryColumn(Column):
    def __call__(self, entry):
        return entry.date

class FlagEntryColumn(Column):
    def __call__(self, entry):
        return entry.flag

# FIXME: These ought to be functions, as in YEAR(date) or MONTH(date), that work
# on both types of entries (they would work on dates, really).
class YearEntryColumn(Column):
    def __call__(self, entry):
        return entry.date.year

class MonthEntryColumn(Column):
    def __call__(self, entry):
        return entry.date.month

ENTRY_COLUMNS = {
    'date'  : DateEntryColumn,
    'flag'  : FlagEntryColumn,
    'year'  : YearEntryColumn,
    'month' : MonthEntryColumn,
    }



def compile_query(select):
    """Prepare an AST for a Select statement into an execution tree.
    The execution tree mostly looks like an AST, but with some nodes
    replaced with the knowledge of an execution context.

    Args:
      select: An instance of query_parser.Select.
    Returns:
      A modified instance of Select with some nodes replaced.
    """
