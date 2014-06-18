"""Views are filters on the global list of entries, which produces a subset of entries.
"""
import datetime

from beancount.core import data
from beancount.ops import summarize
from beancount.core import realization
from beancount.parser import options
from beancount.utils import misc_utils



class View:
    """A container for filtering a subset of entries and realizing that for
    display."""

    def __init__(self, all_entries, options_map, title):

        # A reference to the full list of padded entries.
        self.all_entries = all_entries

        # List of filterered entries for this view, and index at the beginning
        # of the period transactions, past the opening balances. These are
        # computed in _realize().
        self.entries = None
        self.opening_entries = None
        self.closing_entries = None

        # Title.
        self.title = title

        # A reference to the global list of options and the account type names.
        # FIXME: These may be redundant, review whether we actually need these.
        self.options = options_map
        self.account_types = options.get_account_types(options_map)

        # Realization of the filtered entries to display. These are computed in
        # _realize().
        self.real_accounts = None
        self.opening_real_accounts = None
        self.closing_real_accounts = None

        # Realize now, we don't need to do this lazily because we create these
        # view objects on-demand and cache them.
        self._realize()

    def _realize(self):
        """Compute the list of filtered entries and transaction tree."""

        # Get the filtered list of entries.
        self.entries, self.begin_index = self.apply_filter(self.all_entries, self.options)

        if not self.entries:
            self.opening_entries = []
            self.closing_entries = []
        else:
            # Compute the list of entries for the opening balances sheet.
            self.opening_entries = (self.entries[:self.begin_index]
                                    if self.begin_index is not None
                                    else None)


            # Compute the list of entries that includes transfer entries of the
            # income/expenses amounts to the balance sheet's equity (as "net
            # income"). This is used to render the end-period balance sheet, with
            # the current period's net income, closing the period.
            current_accounts = options.get_current_accounts(self.options)
            self.closing_entries = summarize.close(self.entries,
                                                   self.account_types,
                                                   self.options['transfer_currency'],
                                                   *current_accounts)

        # Realize the three sets of entries.
        if self.opening_entries:
            with misc_utils.print_time('realize_opening'):
                self.opening_real_accounts = realization.realize(self.opening_entries,
                                                                 self.account_types)
        else:
            self.opening_real_accounts = None

        with misc_utils.print_time('realize'):
            self.real_accounts = realization.realize(self.entries,
                                                     self.account_types)

        with misc_utils.print_time('realize_closing'):
            self.closing_real_accounts = realization.realize(self.closing_entries,
                                                             self.account_types)

        assert self.real_accounts is not None
        assert self.closing_real_accounts is not None

    def apply_filter(self, entries):
        "Filter the list of entries."
        raise NotImplementedError


class EmptyView(View):
    """An empty view, as a placeholder until we implement one."""

    def __init__(self, entries, options_map, title, *args, **kw):
        View.__init__(self, entries, options_map, title)

    def apply_filter(self, _, __):
        "Return the list of entries unmodified."
        return ([], None)


class AllView(View):

    def apply_filter(self, entries, options_map):
        "Return the list of entries unmodified."
        return (entries, None)


class YearView(View):

    def __init__(self, entries, options_map, title, year):
        self.year = year
        View.__init__(self, entries, options_map, title)

    def apply_filter(self, entries, options_map):
        "Return entries for only that year."

        # Get the transfer account objects.
        previous_accounts = options.get_previous_accounts(options_map)

        # Clamp to the desired period.
        begin_date = datetime.date(self.year, 1, 1)
        end_date = datetime.date(self.year+1, 1, 1)
        account_types = options.get_account_types(options_map)
        with misc_utils.print_time('clamp'):
            entries, index = summarize.clamp(entries, begin_date, end_date,
                                             account_types,
                                             options_map['transfer_currency'],
                                             *previous_accounts)

        return entries, index


class TagView(View):

    def __init__(self, entries, options_map, title, tags):
        # The tags we want to include.
        assert isinstance(tags, (set, list, tuple))
        self.tags = tags

        View.__init__(self, entries, options_map, title)

    def apply_filter(self, entries, options_map):
        "Return only entries with the given tag."

        tags = self.tags
        tagged_entries = [
            entry
            for entry in entries
            if isinstance(entry, data.Transaction) and entry.tags and (entry.tags & tags)]

        return tagged_entries, None


class PayeeView(View):

    def __init__(self, entries, options_map, title, payee):
        # The payee to filter.
        assert isinstance(payee, str)
        self.payee = payee

        View.__init__(self, entries, options_map, title)

    def apply_filter(self, entries, options_map):
        "Return only transactions for the given payee."

        payee = self.payee
        payee_entries = [entry
                         for entry in entries
                         if isinstance(entry, data.Transaction) and (entry.payee == payee)]

        return payee_entries, None


class ComponentView(View):

    def __init__(self, entries, options_map, title, component):
        # The payee to filter.
        assert isinstance(component, str)
        self.component = component

        View.__init__(self, entries, options_map, title)

    def apply_filter(self, entries, options_map):
        "Return only transactions for the given payee."

        component = self.component
        component_entries = [entry
                             for entry in entries
                             if data.has_entry_account_component(entry, component)]

        return component_entries, None
