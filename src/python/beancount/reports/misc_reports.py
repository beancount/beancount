"""Miscellaneous report classes.
"""
import datetime
import re
import io

import dateutil.parser

from beancount.reports import report
from beancount.reports import table
from beancount.reports import tree_table
from beancount.parser import printer
from beancount.parser import options
from beancount.core import data
from beancount.core import realization
from beancount.core import amount
from beancount.core import getters
from beancount.core import account_types
from beancount.ops import prices
from beancount.utils import misc_utils


class ErrorReport(report.Report):
    """Report the errors."""

    names = ['check', 'validate', 'errors']
    default_format = 'text'

    def render_text(self, entries, errors, options_map, file):
        printer.print_errors(errors, file=file)


class PrintReport(report.Report):
    """Print out the entries."""

    names = ['print']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        printer.print_entries(entries, file=file)


class AccountsReport(report.Report):
    """Print out the list of all accounts."""

    names = ['accounts']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        if not entries:
            return

        open_close = getters.get_account_open_close(entries)

        # Render to stdout.
        maxlen = max(len(account) for account in open_close)
        sortkey_fun = account_types.get_account_sort_function(
            options.get_account_types(options_map))
        for account, (open, close) in sorted(open_close.items(),
                                             key=lambda entry: sortkey_fun(entry[0])):
            open_date = open.date if open else ''
            close_date = close.date if close else ''
            file.write('{:{len}}  {}  {}\n'.format(account, open_date, close_date,
                                                   len=maxlen))


class CurrentEventsReport(report.TableReport):
    """Produce a table of the current values of all event types."""

    names = ['current_events', 'latest_events']

    def generate_table(self, entries, errors, options_map):
        events = {}
        for entry in entries:
            if isinstance(entry, data.Event):
                events[entry.type] = entry.description
        return table.create_table([(type_, description)
                                   for type_, description in sorted(events.items())],
                                  [(0, "Type", self.formatter.render_event_type),
                                   (1, "Description")])


class EventsReport(report.TableReport):
    """Produce a table of all the values of a particular event."""

    names = ['events']

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('-e', '--expr',
                            action='store', default=None,
                            help="A regexp to filer on which events to display.")

    def generate_table(self, entries, errors, options_map):
        event_entries = []
        for entry in entries:
            if not isinstance(entry, data.Event):
                continue
            if self.args.expr and not re.match(self.args.expr, entry.type):
                continue
            event_entries.append(entry)
        return table.create_table([(entry.date, entry.type, entry.description)
                                   for entry in event_entries],
                                  [(0, "Date", datetime.date.isoformat),
                                   (1, "Type"),
                                   (2, "Description")])



class ActivityReport(report.HTMLReport,
                     metaclass=report.RealizationMeta):
    """Render the last or recent update activity."""

    names = ['activity', 'updated']

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('--cutoff',
                            action='store', default=None,
                            type=lambda arg_str: dateutil.parser.parse(arg_str).date(),
                            help="Cutoff date where we ignore whatever comes after.")

    def render_real_htmldiv(self, real_root, options_map, file):
        cutoff = self.args.cutoff
        errors = []

        # FIXME(reports): This renders not as a tree, and also the Liabilities table
        # is not the same width. Fix this, this doesn't look good.
        account_types = options.get_account_types(options_map)
        for root in (account_types.assets,
                     account_types.liabilities):
            oss = io.StringIO()
            table = tree_table.tree_table(oss, realization.get(real_root, root),
                                          None,
                                          ['Account', 'Last Entry'])
            num_cells = 0
            for real_account, cells, row_classes in table:
                if not isinstance(real_account, realization.RealAccount):
                    continue
                last_posting = realization.find_last_active_posting(real_account.postings)

                # Don't render updates to accounts that have been closed.
                # Note: this is O(N), maybe we can store this at realization.
                if last_posting is None or isinstance(last_posting, data.Close):
                    continue

                last_date = data.get_entry(last_posting).date

                # Skip this posting if a cutoff date has been specified and the
                # account has been updated to at least the cutoff date.
                if cutoff and cutoff <= last_date:
                    continue

                # Okay, we need to render this. Append.
                cells.append(data.get_entry(last_posting).date
                             if real_account.postings
                             else '-')
                num_cells += 1

            if num_cells:
                file.write(oss.getvalue())



class StatsDirectivesReport(report.TableReport):
    """Render statistics on each directive type, the number of entries by type."""

    names = ['stats-types', 'stats-directives', 'stats-entries']

    def generate_table(self, entries, _, __):
        entries_by_type = misc_utils.groupby(lambda entry: type(entry).__name__,
                                             entries)
        nb_entries_by_type = {name: len(entries)
                              for name, entries in entries_by_type.items()}
        rows = sorted(nb_entries_by_type.items(),
                      key=lambda x: x[1], reverse=True)
        rows = [(name, str(count)) for (name, count) in rows]
        rows.append(('~Total~', str(len(entries))))

        return table.create_table(rows, [(0, 'Type'),
                                         (1, 'Num Entries', '{:>}'.format)])



class StatsPostingsReport(report.TableReport):
    """Render the number of postings for each account."""

    names = ['stats-postings']

    def generate_table(self, entries, _, __):
        all_postings = [posting
                        for entry in entries
                        if isinstance(entry, data.Transaction)
                        for posting in entry.postings]
        postings_by_account = misc_utils.groupby(lambda posting: posting.account,
                                                 all_postings)
        nb_postings_by_account = {key: len(postings)
                                  for key, postings in postings_by_account.items()}
        rows = sorted(nb_postings_by_account.items(), key=lambda x: x[1], reverse=True)
        rows = [(name, str(count)) for (name, count) in rows]
        rows.append(('~Total~', str(sum(nb_postings_by_account.values()))))

        return table.create_table(rows, [(0, 'Account'),
                                         (1, 'Num Postings', '{:>}'.format)])


__reports__ = [
    ErrorReport,
    PrintReport,
    AccountsReport,
    CurrentEventsReport,
    EventsReport,
    ActivityReport,
    StatsDirectivesReport,
    StatsPostingsReport,
    ]
