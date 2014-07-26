"""Views are filters on the global list of entries, which produces a subset of entries.
"""
import datetime
import logging

from beancount.core import data
from beancount.ops import summarize
from beancount.core import realization
from beancount.parser import options
from beancount.utils import misc_utils



def clamp_with_options(entries, begin_date, end_date, options_map):
    """Clamp by getting all the parameters from an options map.

    See clamp() for details.

    Args:
      entries: See clamp().
      begin_date: See clamp().
      end_date: See clamp().
      options_map: A parser's option_map.
    Returns:
      Same as clamp().
    """
    account_types = options.get_account_types(options_map)
    previous_accounts = options.get_previous_accounts(options_map)
    conversion_currency = options_map['conversion_currency']
    return summarize.clamp(entries, begin_date, end_date,
                           account_types,
                           conversion_currency,
                           *previous_accounts)


def close_with_options(entries, options_map):
    """Close by getting all the parameters from an options map.

    See summarize.close() for details.

    Args:
      entries: See close().
      options_map: A parser's option_map.
    Returns:
      Same as close().
    """
    account_types = options.get_account_types(options_map)
    current_accounts = options.get_current_accounts(options_map)
    conversion_currency = options_map['conversion_currency']
    return summarize.close(entries,
                           account_types,
                           conversion_currency,
                           *current_accounts)


class View:
    """A container for filtering a subset of entries and realizing that for
    display."""

    def __init__(self, all_entries, options_map, title):
        """Build a View instance.

        Args:
          all_entries: The full list of directives as output from the loader.
          options_map: The options dict, as output by the parser.
          title: A string, the title of this view to render.
        """

        # A reference to the full list of padded entries.
        self.all_entries = all_entries

        # List of filterered entries for this view, and index at the beginning
        # of the period transactions, past the opening balances. These are
        # computed in _initialize().
        self.entries = None
        self.opening_entries = None
        self.closing_entries = None

        # Title.
        self.title = title

        # Realization of the filtered entries to display. These are computed in
        # _initialize().
        self.real_accounts = None
        self.opening_real_accounts = None
        self.closing_real_accounts = None

        # Realize now, we don't need to do this lazily because we create these
        # view objects on-demand and cache them.
        self._initialize(options_map)

    def _initialize(self, options_map):
        """Compute the list of filtered entries and realization trees."""

        # Get the filtered list of entries.
        self.entries, self.begin_index = self.apply_filter(self.all_entries, options_map)

        # Compute the list of entries for the opening balances sheet.
        self.opening_entries = (self.entries[:self.begin_index]
                                if self.begin_index is not None
                                else [])

        # Compute the list of entries that includes transfer entries of the
        # income/expenses amounts to the balance sheet's equity (as "net
        # income"). This is used to render the end-period balance sheet, with
        # the current period's net income, closing the period.
        self.closing_entries = close_with_options(self.entries, options_map)

        # Realize the three sets of entries.
        account_types = options.get_account_types(options_map)
        with misc_utils.log_time('realize_opening', logging.info):
            self.opening_real_accounts = realization.realize(self.opening_entries,
                                                             account_types)

        with misc_utils.log_time('realize', logging.info):
            self.real_accounts = realization.realize(self.entries,
                                                     account_types)

        with misc_utils.log_time('realize_closing', logging.info):
            self.closing_real_accounts = realization.realize(self.closing_entries,
                                                             account_types)

        assert self.real_accounts is not None
        assert self.closing_real_accounts is not None

    def apply_filter(self, entries):
        """Filter the list of entries.

        This is used to obtain the filtered list of entries.

        Args:
          entries: A list of directives to filter.
        Returns:
          A pair of
            1. a list of filtered entries, and
            2. an integer, the index at which the beginning of the entries for
              the period begin, one directive past the opening
              balances/initialization entries.
        """
        raise NotImplementedError


class EmptyView(View):
    """An empty view, for testing."""

    def __init__(self, entries, options_map, title, *args, **kw):
        """Create an empty view.

        Args:
          entries: A list of directives.
          options_map: A dict of options, as produced by the parser.
          title: A string, the title of this view.
          *args: Ignored.
          **kw: Ignored.
        """
        View.__init__(self, entries, options_map, title)

    def apply_filter(self, _, __):
        "Return the list of entries unmodified."
        return ([], None)


class AllView(View):
    """A view that includes all the entries, unmodified."""

    def apply_filter(self, entries, options_map):
        return (entries, None)


class YearView(View):
    """A view of the entries for just a single year."""

    def __init__(self, entries, options_map, title, year):
        """Create a view clamped to one year.

        Note: this is the only view where the entries are summarized and
        clamped.

        Args:
          entries: A list of directives.
          options_map: A dict of options, as produced by the parser.
          title: A string, the title of this view.
          year: An integer, the year of the exercise period.
        """
        self.year = year
        View.__init__(self, entries, options_map, title)

    def apply_filter(self, entries, options_map):
        # Clamp to the desired period.
        begin_date = datetime.date(self.year, 1, 1)
        end_date = datetime.date(self.year+1, 1, 1)
        with misc_utils.log_time('clamp', logging.info):
            entries, index = clamp_with_options(entries, begin_date, end_date, options_map)
        return entries, index


class TagView(View):
    """A view that includes only entries some specific tags."""

    def __init__(self, entries, options_map, title, tags):
        """Create a view with only entries tagged with the given tags.

        Note: this is the only view where the entries are summarized and
        clamped.

        Args:
          entries: A list of directives.
          options_map: A dict of options, as produced by the parser.
          title: A string, the title of this view.
          tags: A set of strings, the tags to include. Entries with at least
            one of these tags will be included in the output.
        """
        assert isinstance(tags, (set, list, tuple))
        self.tags = tags
        View.__init__(self, entries, options_map, title)

    def apply_filter(self, entries, options_map):
        tags = self.tags
        tagged_entries = [
            entry
            for entry in entries
            if isinstance(entry, data.Transaction) and entry.tags and (entry.tags & tags)]

        return tagged_entries, None


class PayeeView(View):
    """A view that includes entries with some specific payee."""

    def __init__(self, entries, options_map, title, payee):
        """Create a view clamped to one year.

        Note: this is the only view where the entries are summarized and
        clamped.

        Args:
          entries: A list of directives.
          options_map: A dict of options, as produced by the parser.
          title: A string, the title of this view.
          payee: A string, the payee whose transactions to include.
        """
        assert isinstance(payee, str)
        self.payee = payee
        View.__init__(self, entries, options_map, title)

    def apply_filter(self, entries, options_map):
        payee = self.payee
        payee_entries = [entry
                         for entry in entries
                         if isinstance(entry, data.Transaction) and (entry.payee == payee)]

        return payee_entries, None


class ComponentView(View):
    """A view that includes transactions with at least one posting with an account
    that includes a given component."""

    def __init__(self, entries, options_map, title, component):
        """Create a view clamped to one year.

        Note: this is the only view where the entries are summarized and
        clamped.

        Args:
          entries: A list of directives.
          options_map: A dict of options, as produced by the parser.
          title: A string, the title of this view.
          compnent: A string, the name of an account component to include.
        """
        assert isinstance(component, str)
        self.component = component
        View.__init__(self, entries, options_map, title)

    def apply_filter(self, entries, options_map):
        component = self.component
        component_entries = [entry
                             for entry in entries
                             if data.has_entry_account_component(entry, component)]

        return component_entries, None
