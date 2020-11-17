"""Miscellaneous report classes.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import re
import io
import textwrap
import functools

from beancount.reports import base
from beancount.utils import table
from beancount.reports import tree_table
from beancount.parser import printer
from beancount.parser import options
from beancount.core import display_context
from beancount.core import data
from beancount.core import realization
from beancount.core import getters
from beancount.core import account_types
from beancount.utils import misc_utils
from beancount.utils import date_utils


class NoopReport(base.Report):
    """Report nothing."""

    names = ['check', 'validate']
    default_format = 'text'

    def render_text(self, entries, errors, options_map, file):
        pass


class ErrorReport(base.HTMLReport):
    """Report the errors."""

    names = ['errors']
    default_format = 'text'

    def render_text(self, entries, errors, options_map, file):
        printer.print_errors(errors, file=file)

    def render_htmldiv(self, entries, errors, options_map, file):
        dcontext = options_map['dcontext']
        file.write('<div id="errors">\n')
        for error in errors:
            file.write('<div class="error">\n')
            if hasattr(error, 'source'):
                file.write('<a class="source" href="{}">{}</a><br/>\n'.format(
                    self.formatter.render_source(error.source),
                    printer.render_source(error.source)))
            file.write('<span class="error-message">{}</span>\n'.format(
                error.message))
            if error.entry is not None:
                file.write('<pre class="syntax">\n')
                file.write(textwrap.indent(
                    printer.format_entry(error.entry, dcontext), '  '))
                file.write('</pre>\n')
            file.write('</div>\n')
        file.write('</div>\n')


class PrintReport(base.Report):
    """Print out the entries."""

    names = ['print']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        # Create a context that renders all numbers with their natural
        # precision, but honors the commas option. This is kept in sync with
        # {2c694afe3140} to avoid a dependency.
        dcontext = display_context.DisplayContext()
        dcontext.set_commas(options_map['dcontext'].commas)
        printer.print_entries(entries, dcontext, file=file)


class AccountsReport(base.Report):
    """Print out the list of all accounts."""

    names = ['accounts']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        if not entries:
            return

        open_close = getters.get_account_open_close(entries)

        # Render to stdout.
        maxlen = (max(len(account) for account in open_close) if open_close else 0)
        sortkey_fun = functools.partial(account_types.get_account_sort_key,
                                        options.get_account_types(options_map))
        for account, (open, close) in sorted(open_close.items(),
                                             key=lambda entry: sortkey_fun(entry[0])):
            open_date = open.date if open else ''
            close_date = close.date if close else ''
            file.write('{:{len}}  {}  {}\n'.format(account, open_date, close_date,
                                                   len=maxlen))


class CurrentEventsReport(base.TableReport):
    """Produce a table of the current values of all event types."""

    names = ['current_events', 'latest_events']

    def generate_table(self, entries, errors, options_map):
        events = {}
        for entry in entries:
            if isinstance(entry, data.Event):
                events[entry.type] = entry.description
        return table.create_table(list(sorted(events.items())),
                                  [(0, "Type", self.formatter.render_event_type),
                                   (1, "Description")])


class EventsReport(base.TableReport):
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



class ActivityReport(base.HTMLReport,
                     metaclass=base.RealizationMeta):
    """Render the last or recent update activity."""

    names = ['activity', 'updated']
    default_format = 'text'

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('-d', '--cutoff',
                            action='store', default=None,
                            type=date_utils.parse_date_liberally,
                            help="Cutoff date where we ignore whatever comes after.")

    def render_real_text(self, real_root, price_map, price_date, options_map, file):
        rows = []
        account_types = options.get_account_types(options_map)
        for root in (account_types.assets,
                     account_types.liabilities):
            for unused_first_line, unused_cont_line, real_account in realization.dump(
                    realization.get(real_root, root)):

                last_posting = realization.find_last_active_posting(
                    real_account.txn_postings)

                # Don't render updates to accounts that have been closed.
                # Note: this is O(N), maybe we can store this at realization.
                if last_posting is None or isinstance(last_posting, data.Close):
                    continue

                last_date = data.get_entry(last_posting).date
                # Skip this posting if a cutoff date has been specified and the
                # account has been updated to at least the cutoff date.
                if self.args.cutoff and self.args.cutoff <= last_date:
                    continue

                rows.append((real_account.account, last_date))

        table_ = table.create_table(rows, [(0, 'Account'), (1, 'Last Date', '{}'.format)])
        table.render_table(table_, file, 'text')

    def render_real_htmldiv(self, real_root, price_map, price_date, options_map, file):
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
                last_posting = realization.find_last_active_posting(
                    real_account.txn_postings)

                # Don't render updates to accounts that have been closed.
                # Note: this is O(N), maybe we can store this at realization.
                if last_posting is None or isinstance(last_posting, data.Close):
                    continue

                last_date = data.get_entry(last_posting).date

                # Skip this posting if a cutoff date has been specified and the
                # account has been updated to at least the cutoff date.
                if self.args.cutoff and self.args.cutoff <= last_date:
                    continue

                # Okay, we need to render this. Append.
                cells.append(data.get_entry(last_posting).date
                             if real_account.txn_postings
                             else '-')
                num_cells += 1

            if num_cells:
                file.write(oss.getvalue())



class StatsDirectivesReport(base.TableReport):
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



class StatsPostingsReport(base.TableReport):
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
    NoopReport,
    ErrorReport,
    PrintReport,
    AccountsReport,
    CurrentEventsReport,
    EventsReport,
    ActivityReport,
    StatsDirectivesReport,
    StatsPostingsReport,
    ]
